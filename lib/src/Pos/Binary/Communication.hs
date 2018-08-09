{-# LANGUAGE BinaryLiterals #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Communication-related serialization -- messages mostly.

module Pos.Binary.Communication
    ( serializeMsgSerializedBlock
    , serializeMsgStreamBlock
    ) where

import           Universum

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word32)

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), decodeKnownCborDataItem,
                                   decodeUnknownCborDataItem, deriveSimpleBi,
                                   encodeKnownCborDataItem, encodeListLen,
                                   encodeUnknownCborDataItem, enforceSize,
                                   serialize, serialize')
import           Pos.Binary.Core ()
import           Pos.Block.BHelpers ()
import           Pos.Block.Network (MsgBlock (..), MsgSerializedBlock (..), MsgGetBlocks (..), MsgGetHeaders (..),
                                    MsgHeaders (..), MsgStream (..), MsgStreamStart (..),
                                    MsgStreamUpdate (..), MsgStreamBlock (..))
import           Pos.Core (BlockVersion, HeaderHash)
import           Pos.DB.Class (Serialized (..))
import           Pos.Infra.Communication.Types.Protocol (HandlerSpec (..),
                                                         HandlerSpecs,
                                                         MsgSubscribe (..),
                                                         MsgSubscribe1 (..),
                                                         VerInfo (..))
import           Pos.Util.Util (cborError)

-- TODO: move into each component

----------------------------------------------------------------------------
-- Blocks
----------------------------------------------------------------------------

deriveSimpleBi ''MsgGetHeaders [
    Cons 'MsgGetHeaders [
        Field [| mghFrom :: [HeaderHash]     |],
        Field [| mghTo   :: Maybe HeaderHash |]
    ]]

deriveSimpleBi ''MsgGetBlocks [
    Cons 'MsgGetBlocks [
        Field [| mgbFrom :: HeaderHash |],
        Field [| mgbTo   :: HeaderHash |]
    ]]

instance Bi MsgHeaders where
    encode = \case
        (MsgHeaders b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgNoHeaders t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgHeaders" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgHeaders <$> decode
            1 -> MsgNoHeaders <$> decode
            t -> cborError $ "MsgHeaders wrong tag: " <> show t

instance Bi MsgBlock where
    encode = \case
        (MsgBlock b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgNoBlock t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgBlock <$> decode
            1 -> MsgNoBlock <$> decode
            t -> cborError $ "MsgBlock wrong tag: " <> show t

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgBlock b) = serializeMsgSerializedBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgSerializedBlock :: MsgSerializedBlock -> BS.ByteString
serializeMsgSerializedBlock (MsgSerializedBlock b) = "\x82\x0" <> unSerialized b
serializeMsgSerializedBlock (MsgNoSerializedBlock t) = serialize' (MsgNoBlock t)

-- Serialize `MsgSerializedBlock` with the property
-- ```
-- serialize (MsgStreamBlock b) = serializeMsgStreamBlock (MsgSerializedBlock $ serialize b)
-- ```
serializeMsgStreamBlock :: MsgSerializedBlock -> LBS.ByteString
serializeMsgStreamBlock (MsgSerializedBlock b)   = "\x82\x0" <> LBS.fromStrict (unSerialized b)
serializeMsgStreamBlock (MsgNoSerializedBlock t) = serialize (MsgStreamNoBlock t)

deriveSimpleBi ''MsgStreamStart [
    Cons 'MsgStreamStart [
        Field [| mssFrom   :: [HeaderHash] |],
        Field [| mssTo     :: HeaderHash |],
        Field [| mssWindow :: Word32 |]
    ]]

deriveSimpleBi ''MsgStreamUpdate [
    Cons 'MsgStreamUpdate [
        Field [| msuWindow :: Word32 |]
    ]]

instance Bi MsgStream where
    encode = \case
        (MsgStart s)  -> encodeListLen 2 <> encode (0 :: Word8) <> encode s
        (MsgUpdate u) -> encodeListLen 2 <> encode (1 :: Word8) <> encode u
    decode = do
        enforceSize "MsgStream" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgStart  <$> decode
            1 -> MsgUpdate <$> decode
            t -> cborError $ "MsgStream wrong tag: " <> show t

instance Bi MsgStreamBlock where
    encode = \case
        (MsgStreamBlock b) -> encodeListLen 2 <> encode (0 :: Word8) <> encode b
        (MsgStreamNoBlock t) -> encodeListLen 2 <> encode (1 :: Word8) <> encode t
        MsgStreamEnd -> encodeListLen 2 <> encode (2 :: Word8) <> encode (0 :: Word8)
    decode = do
        enforceSize "MsgBlock" 2
        tag <- decode @Word8
        case tag of
            0 -> MsgStreamBlock <$> decode
            1 -> MsgStreamNoBlock <$> decode
            2 -> do
                 (_ :: Word8 )<- decode
                 pure MsgStreamEnd
            t -> cborError $ "MsgStreamBlock wrong tag: " <> show t

-- deriveSimpleBi is not happy with constructors without arguments
-- "fake" deriving as per `MempoolMsg`.
-- TODO: Shall we encode this as `CBOR` TkNull?
instance Bi MsgSubscribe1 where
    encode MsgSubscribe1 = encode (42 :: Word8)
    decode = decode @Word8 >>= \case
        42 -> pure MsgSubscribe1
        n  -> cborError $ "MsgSubscribe1 wrong byte:" <> show n

instance Bi MsgSubscribe where
    encode = \case
        MsgSubscribe          -> encode (42 :: Word8)
        MsgSubscribeKeepAlive -> encode (43 :: Word8)
    decode = decode @Word8 >>= \case
        42 -> pure MsgSubscribe
        43 -> pure MsgSubscribeKeepAlive
        n  -> cborError $ "MsgSubscribe wrong byte: " <> show n

----------------------------------------------------------------------------
-- Protocol version info and related
----------------------------------------------------------------------------

instance Bi HandlerSpec where
    encode input = case input of
        ConvHandler mname        ->
            encodeListLen 2 <> encode (0 :: Word8) <> encodeKnownCborDataItem mname
        UnknownHandler word8 bs  ->
            encodeListLen 2 <> encode word8 <> encodeUnknownCborDataItem (LBS.fromStrict bs)
    decode = do
        enforceSize "HandlerSpec" 2
        tag <- decode @Word8
        case tag of
          0 -> ConvHandler        <$> decodeKnownCborDataItem
          _ -> UnknownHandler tag <$> decodeUnknownCborDataItem

deriveSimpleBi ''VerInfo [
    Cons 'VerInfo [
        Field [| vIMagic        :: Int32        |],
        Field [| vIBlockVersion :: BlockVersion |],
        Field [| vIInHandlers   :: HandlerSpecs |],
        Field [| vIOutHandlers  :: HandlerSpecs |]
    ]]
