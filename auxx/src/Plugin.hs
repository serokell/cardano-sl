{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

-- | Auxx plugin.

module Plugin
       ( auxxPlugin
       , ppValue
       , rawExec
       ) where

import           Universum

#if !(defined(mingw32_HOST_OS))
import           System.Exit (ExitCode (ExitSuccess))
import           System.Posix.Process (exitImmediately)
#endif
import           Control.Monad.Except (ExceptT (..), withExceptT)
import           Data.Constraint (Dict (..))
import           Data.Constraint (Dict (..))
import           Formatting (build, char, float, int, sformat, stext, (%))
import           Formatting (float, int, sformat, (%))
import           Mockable (Delay, Mockable, delay)
import           Serokell.Util (sec)
import           System.IO (hFlush, stdout)
import           System.Wlog (CanLog, HasLoggerName, logInfo)

import           Pos.Communication (OutSpecs (..))
import           Pos.Core (AddrStakeDistribution, Address, ApplicationName (..), BlockVersion,
                           CoinPortion, SoftwareVersion (..), StakeholderId)
-- import           Pos.Core.Update.Types (ApplicationName (..))
import           Pos.Core.Common (CoinPortion (..))
import           Pos.Core.Txp (TxOut (..))
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Txp (genesisUtxo, unGenesisUtxo)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Worker.Types (WorkerSpec, worker)

import           AuxxOptions (AuxxOptions (..))
import           Command (createCommandProcs)
import qualified Lang
import           Mode (MonadAuxxMode)
import           Repl (PrintAction, WithCommandAction (..))

----------------------------------------------------------------------------
-- Plugin implementation
----------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

auxxPlugin ::
       (HasCompileInfo, MonadAuxxMode m, Mockable Delay m)
    => AuxxOptions
    -> Either WithCommandAction Text
    -> (WorkerSpec m, OutSpecs)
auxxPlugin auxxOptions repl = worker mempty $ \diffusion -> do
    logInfo $ sformat ("Length of genesis utxo: " %int)
                      (length $ unGenesisUtxo genesisUtxo)
    rawExec (Just Dict) auxxOptions (Just diffusion) repl

rawExec ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> AuxxOptions
    -> Maybe (Diffusion m)
    -> Either WithCommandAction Text
    -> m ()
rawExec mHasAuxxMode AuxxOptions{..} mDiffusion = \case
    Left WithCommandAction{..} -> do
        printAction "... the auxx plugin is ready"
        forever $ withCommand $ runCmd mHasAuxxMode mDiffusion printAction
    Right cmd -> runWalletCmd mHasAuxxMode mDiffusion cmd

runWalletCmd ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> Maybe (Diffusion m)
    -> Text
    -> m ()
runWalletCmd mHasAuxxMode mDiffusion line = do
    runCmd mHasAuxxMode mDiffusion printAction line
    printAction "Command execution finished"
    printAction " " -- for exit by SIGPIPE
    liftIO $ hFlush stdout
#if !(defined(mingw32_HOST_OS))
    delay $ sec 3
    liftIO $ exitImmediately ExitSuccess
#endif
  where
    printAction = putText

runCmd ::
       ( HasCompileInfo
       , MonadIO m
       , MonadCatch m
       , CanLog m
       , HasLoggerName m
       , Mockable Delay m
       )
    => Maybe (Dict (MonadAuxxMode m))
    -> Maybe (Diffusion m)
    -> PrintAction m
    -> Text
    -> m ()
runCmd mHasAuxxMode mDiffusion printAction line = do
    let commandProcs = createCommandProcs (Just Dict) mHasAuxxMode printAction mDiffusion
        parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
        resolveCommandProcs =
            withExceptT Lang.ppResolveErrors . ExceptT . return .
            Lang.resolveCommandProcs commandProcs
        evaluate = withExceptT Lang.ppEvalError . ExceptT . Lang.evaluate
        pipeline = parse >=> resolveCommandProcs >=> evaluate
    runExceptT (pipeline line) >>= \case
        Left errDoc -> printAction (Lang.renderAuxxDoc errDoc)
        Right value -> (printAction . ppValue) value


ppValue :: Lang.Value -> Text
ppValue = \case
    Lang.ValueUnit -> ""
    Lang.ValueNumber n -> (sformat float n)
    Lang.ValueString s -> sformat (char % stext % char) '\"' (toText s) '\"'
    Lang.ValueBool b -> printBool b
    Lang.ValueAddress a ->  (pretty a)
    Lang.ValuePublicKey pk ->  (sformat fullPublicKeyF pk)
    Lang.ValueTxOut txOut -> (pretty txOut)
    Lang.ValueStakeholderId sId ->  (sformat hashHexF sId)
    Lang.ValueHash h ->  (sformat hashHexF (getAHash h))
    Lang.ValueBlockVersion v ->  (pretty v)
    Lang.ValueSoftwareVersion v -> printSoftware v
    Lang.ValueBlockVersionModifier bvm ->  (pretty bvm)
    Lang.ValueBlockVersionData bvd ->  printBVD bvd
    Lang.ValueProposeUpdateSystem pus ->  (show pus)
    Lang.ValueAddrDistrPart adp ->  printAddrDistrPart adp
    Lang.ValueAddrStakeDistribution asd ->  (pretty asd)
    Lang.ValueFilePath s ->  (toText s)
    Lang.ValueList vs -> foldMap ((mappend "  ") . ppValue) vs

-- need to implement printCommand with polymorphic input:
-- BlockVersionModifier or
-- BlockVersionData or
-- ProposeUpdateSystem or
-- AddrDistrPart or
-- AddrStakeDistribution or
-- TxOut -> Text
printBool :: Bool -> Text
printBool True  = "true"
printBool False = "false"

printSoftware :: SoftwareVersion -> Text
printSoftware SoftwareVersion {..} =
    sformat ("software name: "%char%stext%char%" n: "%build) '\"' (getApplicationName svAppName) '\"' svNumber

printAddrDistrPart :: Lang.AddrDistrPart -> Text
printAddrDistrPart (Lang.AddrDistrPart sId cp) =
    sformat ("dp s: "%char%stext%char%" p: "%build) '\"' (sformat hashHexF sId) '\"' (getCoinPortion cp)

printBVD :: BlockVersionData -> Text
printBVD bvd = sformat ("bvd-read value: "%char%stext%char) '\"' (show bvd) '\"'


-- printTxOut :: TxOut -> Text
-- printTxOut TxOut {..} = sformat ("tx-out addr: "%stext %" value " %stext) (show txOutAddress) (show txOutValue)

----------------------------------------------------------------------------
-- Extra logging
----------------------------------------------------------------------------

-- This addLogging was misplaced to begin with.
-- A debug-mode diffusion layer could be chosen, which logs absolutely all
-- network activity. But surely for auxx logging, the logging should go around
-- the high-level auxx commands, no?
{-
addLogging :: forall m. WithLogger m => SendActions m -> SendActions m
addLogging SendActions{..} = SendActions{
      enqueueMsg = error "unused"
    , withConnectionTo = aux
    }
  where
    aux nid k = withConnectionTo nid $ \peerData -> fmap auxConv (k peerData)
    auxConv (Conversation k) = Conversation (\acts -> k (auxActs acts))

    auxActs :: (Message snd, Message rcv)
            => ConversationActions snd rcv m -> ConversationActions snd rcv m
    auxActs (ConversationActions{..}) = ConversationActions {
        send = \body -> do
                 logDebug $ sformat ("Auxx sending " % stext) (formatMessage body)
                 send body
      , recv = \limit -> do
                 mRcv <- recv limit
                 logDebug $
                   case mRcv of
                     Nothing  -> sformat ("Auxx received end of input")
                     Just rcv -> sformat ("Auxx received " % stext) (formatMessage rcv)
                 return mRcv
      , sendRaw = sendRaw
      }
-}
