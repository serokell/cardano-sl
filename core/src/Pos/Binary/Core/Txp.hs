{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Binary serialization of core Txp types.

module Pos.Binary.Core.Txp
       (
       ) where

import           Universum

import           Pos.Binary.Class (Bi (..), Cons (..), Field (..), deriveSimpleBi, encodeListLen,
                                   enforceSize)
import           Pos.Binary.Core.Script ()
import           Pos.Binary.Merkle ()
import qualified Pos.Core.Txp as T

----------------------------------------------------------------------------
-- Core
----------------------------------------------------------------------------

deriveSimpleBi ''T.TxOutAux [
    Cons 'T.TxOutAux [
        Field [| T.toaOut   :: T.TxOut |]
    ]]

instance Bi T.TxSigData where
    encode (T.TxSigData {..}) = encode txSigTxHash
    decode = T.TxSigData <$> decode

instance Bi T.TxProof where
    encode proof =  encodeListLen 3
                 <> encode (T.txpNumber proof)
                 <> encode (T.txpRoot proof)
                 <> encode (T.txpWitnessesHash proof)
    decode = do
        enforceSize "TxProof" 3
        T.TxProof <$> decode <*>
                      decode <*>
                      decode

instance Bi T.TxPayload where
    encode T.UnsafeTxPayload {..} = encode $ zip (toList _txpTxs) _txpWitnesses
    decode = T.mkTxPayload <$> decode
