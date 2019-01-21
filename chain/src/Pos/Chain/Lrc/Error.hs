module Pos.Chain.Lrc.Error
       (
         LrcError (..)
       ) where

import           Universum

import           Formatting (bprint, build, int, stext, (%))
import           Formatting.Buildable (Buildable)
import qualified Formatting.Buildable

import           Pos.Core.Slotting (EpochIndex)

data LrcError
    = LrcDataUnknown !EpochIndex !Text
    | UnknownBlocksForLrc
    | CanNotReuseSeedForLrc !EpochIndex
    | LrcAfterGenesis
    deriving (Show)

instance Exception LrcError

instance Buildable LrcError where
    build (LrcDataUnknown epoch reason) =
        bprint
            ("LRC data isn't presented for epoch #"%int%
             " so raise the exception with reason: "%stext)
            epoch reason
    build UnknownBlocksForLrc =
        bprint "there are no blocks for LRC computation"
    build (CanNotReuseSeedForLrc epoch) =
        bprint ("LRC attempted to reuse seed from previous epoch "%
                "(i.e. epoch "%build%"), but the seed wasn't in the db") epoch
    build LrcAfterGenesis =
        bprint "LRC was attempted after adoption of genesis block"
