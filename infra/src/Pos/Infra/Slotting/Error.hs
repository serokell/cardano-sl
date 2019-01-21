-- | Run-time errors in Slotting.

module Pos.Infra.Slotting.Error
       ( SlottingError (..)
       ) where

import           Universum

import           Control.Exception.Safe (Exception (..))
import           Fmt (pretty)
import           Formatting (bprint, (%))
import           Formatting.Buildable (Buildable)
import qualified Formatting.Buildable

import           Pos.Core.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)
import           Pos.Core.Slotting (SlotId, slotIdF)

-- | Type aggregating run-time errors related to Slotting.
data SlottingError = SEUnknownSlotStart !SlotId
  deriving (Show, Typeable)

instance Buildable SlottingError where
    build (SEUnknownSlotStart slot) =
        bprint ("start of "%slotIdF%" is surprisingly unknown") slot

instance Exception SlottingError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = pretty
