module Pos.Chain.Ssc.Opening
       ( Opening (..)
       ) where

import           Universum

import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting.Buildable (Buildable)

import           Pos.Binary.Class (AsBinary, Bi (..))
import           Pos.Crypto (Secret)

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: AsBinary Secret
    } deriving (Show, Eq, Generic, Buildable, NFData)

instance Bi Opening where
    encode = encode . getOpening
    decode = Opening <$> decode

deriveSafeCopySimple 0 'base ''Opening
