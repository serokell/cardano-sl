{-# LANGUAGE RankNTypes #-}
module Test.Auxx.Printer.PrinterSpec
       ( spec
       ) where

import           Universum

import           Command (createCommandProcs)
import           Control.Monad.Except (ExceptT (..), withExceptT)

-- import           Data.Constraint (Dict(..))
-- import           Mode (MonadAuxxMode, AuxxMode)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (SpecWith, Arg, Expectation, Spec, describe, it, shouldBe)
import           Test.QuickCheck (Arbitrary (..), generate)
import           Data.Functor.Identity (Identity)
-- import           Formatting (float, int, char, stext, sformat, fprint, (%)) --need
import           Plugin (ppValue)
import           Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Lang.Value (Value (..))


import qualified Lang as Lang

spec :: Spec
spec = describe "Auxx.Repl.ppValue" $ do
    traverse_ itHandles values

itHandles :: Value -> SpecWith (Arg Expectation)
itHandles val = it ("handles " <> show val) $ unitPrinter val

unitPrinter :: Value -> Expectation
unitPrinter val = runIdentity $ unitPrinterMonadIO
  where
    unitPrinterMonadIO :: Identity Expectation 
    unitPrinterMonadIO = do
        eithParsed <- (parseBack . ppValue) val
        return $ eithParsed `shouldBe` (Right val)
      where
        parseBack :: Text -> Identity (Either Doc Value)
        parseBack line = withCompileInfo $(retrieveCompileTimeInfo) $ do
            let
                printAction line = return ()
                commandProcs = createCommandProcs Nothing Nothing printAction Nothing

                parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
                resolveCommandProcs =
                    withExceptT Lang.ppResolveErrors . ExceptT . return .
                    Lang.resolveCommandProcs commandProcs
                evaluate = withExceptT Lang.ppEvalError . ExceptT . Lang.evaluate
                pipeline = parse >=> resolveCommandProcs >=> evaluate
            runExceptT (pipeline line) 

instance Eq Doc

data RunErr = ParseError | EvalError deriving (Eq, Show)
instance Exception RunErr 

values :: [Value]
values = [ ValueNumber $ genUnsafe arbitrary
         , ValueString $ genUnsafe arbitrary
         , ValueAddress $ genUnsafe arbitrary
         , ValueBool  $ genUnsafe arbitrary
         , ValueFilePath  $ genUnsafe arbitrary
         , ValuePublicKey  $ genUnsafe arbitrary
         , ValueStakeholderId  $ genUnsafe arbitrary
         , ValueHash  $ genUnsafe arbitrary
         , ValueBlockVersion  $ genUnsafe arbitrary
         , ValueSoftwareVersion  $ genUnsafe arbitrary
         -- to be implemented
        --  , ValueBlockVersionModifier BlockVersionModifier
        --  , ValueBlockVersionData BlockVersionData
        --  , ValueProposeUpdateSystem ProposeUpdateSystem
        --  , ValueAddrDistrPart AddrDistrPart
        --  , ValueAddrStakeDistribution AddrStakeDistribution
        --  , ValueTxOut TxOut
        --  , ValueList [Value]
         ]
  where
    genUnsafe = unsafePerformIO . generate