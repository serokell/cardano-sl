-- | Types describing runtime errors related to Txp.

module Pos.Chain.Txp.Error
       ( TxpError (..)
       ) where

import           Control.Exception.Safe (Exception (..))
import           Formatting (bprint, stext, (%))
import           Formatting.Buildable (Buildable)
import           Fmt (pretty)
import qualified Formatting.Buildable
import           Universum

import           Pos.Core.Exception (cardanoExceptionFromException,
                     cardanoExceptionToException)

data TxpError
    = TxpInternalError !Text
    -- ^ Something bad happened inside Txp
    deriving (Show)

instance Exception TxpError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString @Text . pretty

instance Buildable TxpError where
    build (TxpInternalError msg) =
        bprint ("internal error in Transaction processing: "%stext) msg
