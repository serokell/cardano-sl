-- | Convenient TCP transport acquire and release with common configuration
-- options.

module Pos.Diffusion.Transport.TCP
    ( bracketTransportTCP
    ) where

import           Universum

import           Data.Time.Units (Microsecond)
import           Formatting (sformat, shown, (%))
import           System.Wlog (WithLogger, askLoggerName, logError, usingLoggerName)

import           Network.QDisc.Fair (fairQDisc)
-- import qualified Network.Transport as NT (closeTransport)
import           Network.Transport.Abstract (Transport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP

bracketTransportTCP
    :: ( MonadIO m
       , MonadIO n
       , MonadThrow m
       , MonadMask m
       , WithLogger m
       )
    => Microsecond
    -> TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransportTCP connectionTimeout tcpAddr k = bracket
    (createTransportTCP connectionTimeout tcpAddr)
    snd
    (k . fst)

createTransportTCP
    :: ( MonadIO n
       , MonadIO m
       , WithLogger m
       , MonadThrow m
       )
    => Microsecond -- ^ Connection timeout
    -> TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP connectionTimeout addrInfo = do
    loggerName <- askLoggerName
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral connectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                     usingLoggerName (loggerName <> "transport") $
                         logError $ sformat ("Exception in tcp server: " % shown) e
             })
    transportE <- liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throwM e
        -- It turned out (see AD-101) that 'closeTransport' sometimes
        -- hangs (mostly in ariadne-qt case). We don't know concrete
        -- reasons why it happens and as a workaround we just don't
        -- call that function. It's not too bad, because this code can
        -- be called only when the application is being closed and
        -- nobody guarantees such code will be called anyway.
        Right transport -> return (concrete transport, pass)
        -- Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)
