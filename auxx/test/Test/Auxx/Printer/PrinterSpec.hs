{-# LANGUAGE RankNTypes #-}
module Test.Auxx.Printer.PrinterSpec
       ( spec
       ) where

import           Universum

import           Command (createCommandProcs)
import           Control.Monad.Except (ExceptT (..), withExceptT)
import           Data.Functor.Identity (Identity)
import           Lang.Value (AddrDistrPart (..), Value (..))
-- import           Data.Constraint (Dict(..))
-- import           Mode (MonadAuxxMode, AuxxMode)
import           Plugin (ppValue)
import           Printer (pprExpr)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Arg, Expectation, Spec, SpecWith, describe, it, shouldBe)
import           Test.QuickCheck (Arbitrary (..), generate)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
-- import           Formatting (float, int, char, stext, sformat, fprint, (%)) --need
import           Lang (Expr, Name)
import           Pos.Core (SoftwareVersion (..))
import           Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)


import qualified Lang as Lang
import qualified Text.PrettyPrint.ANSI.Leijen as PP

spec :: Spec
spec = describe "Auxx.Repl.ppValue" $ do
    traverse_ itHandles expressions

itHandles :: (Expr Name) -> SpecWith (Arg Expectation)
itHandles val = it ("handles " <> show val) $ exprPrinter val

-- unitPrinter :: Value -> Expectation
-- unitPrinter val = runIdentity $ unitPrinterMonadIO
--   where
--     unitPrinterMonadIO :: Identity Expectation
--     unitPrinterMonadIO = do
--         eithParsed <- (parseBack . ppValue) val
--         return $ eithParsed `shouldBe` (Right val)
--       where
--         parseBack :: Text -> Identity (Either Doc Value)
--         parseBack line = withCompileInfo $(retrieveCompileTimeInfo) $ do
--             let
--                 printAction line = return ()
--                 commandProcs = createCommandProcs Nothing Nothing printAction Nothing

--                 parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
--                 resolveCommandProcs =
--                     withExceptT Lang.ppResolveErrors . ExceptT . return .
--                     Lang.resolveCommandProcs commandProcs
--                 evaluate = withExceptT Lang.ppEvalError . ExceptT . Lang.evaluate
--                 pipeline = parse >=> resolveCommandProcs >=> evaluate
--             runExceptT (pipeline line) >>= \case
--                 Left errDoc ->
--                     let
--                         errMsg = ((text . toString) line PP.<$> errDoc)
--                     in return $ Left errMsg
--                 Right val -> return $ Right val

exprPrinter :: Expr Name -> Expectation
exprPrinter expr = runIdentity $ exprPrinterId
  where
    exprPrinterId :: Identity Expectation
    exprPrinterId = do
        eithParsed <- (parseBack . pprExpr) expr
        return $ eithParsed `shouldBe` (Right expr)

parseBack :: Text -> Identity (Either Doc (Expr Name))
parseBack line = withCompileInfo $(retrieveCompileTimeInfo) $ do
    let
        printAction line = return ()
        commandProcs = createCommandProcs Nothing Nothing printAction Nothing :: [Lang.CommandProc Identity]

        parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
    runExceptT (parse line) >>= \case
        Left errDoc ->
            let
                errMsg = ((text . toString) line PP.<$> errDoc)
            in return $ Left errMsg
        Right expr -> return $ Right expr

expressions :: [Expr Name]
expressions = [ Lang.ExprUnit
              , Lang.ExprLit (Lang.LitNumber 555)
              , Lang.ExprGroup ((Lang.ExprLit (Lang.LitNumber 555)):|[Lang.ExprLit (Lang.LitHash $ genUnsafe arbitrary)])
              , Lang.ExprGroup ((Lang.ExprLit (Lang.LitNumber 555)):|[Lang.ExprProcCall procCall])
              , Lang.ExprGroup ((Lang.ExprLit (Lang.LitNumber 555)):|[Lang.ExprProcCall procCallWithFunc])
              , Lang.ExprLit (Lang.LitString "jjl")
              , Lang.ExprLit (Lang.LitAddress $ genUnsafe arbitrary)
              , Lang.ExprLit (Lang.LitPublicKey $ genUnsafe arbitrary)
              , Lang.ExprLit (Lang.LitHash $ genUnsafe arbitrary)
              , Lang.ExprLit (Lang.LitBlockVersion $ genUnsafe arbitrary)
              , Lang.ExprLit (Lang.LitFilePath "/kkk")]
  where
    genUnsafe = unsafePerformIO . generate

    procCall = Lang.ProcCall "foo" [Lang.ArgKw "argName" (Lang.ExprLit (Lang.LitString "argValue")), (Lang.ArgPos (Lang.ExprLit (Lang.LitString "posValue")))]
    procCallWithFunc = Lang.ProcCall "foo" [Lang.ArgKw "argName" (Lang.ExprLit (Lang.LitString "argValue")), (Lang.ArgPos (Lang.ExprProcCall procCall))]
-- (LitNumber a) = (sformat float a)
-- f          (LitString a) = sformat (char % stext % char) '\"' (toText a) '\"'
-- f         (LitAddress a) = (pretty a)
-- f       (LitPublicKey a) = (sformat fullPublicKeyF a)
-- f   (LitStakeholderId a) = (sformat hashHexF a)
-- f            (LitHash a) = (sformat hashHexF (getAHash a))
-- f    (LitBlockVersion a) = (pretty a)
-- f (LitSoftwareVersion a) = printSoftware a
-- f        (LitFilePath a)

printTest :: IO ()
printTest = forM_ expressions $ \ex -> do
    putText $ pprExpr ex
    putText ""

instance Eq Doc

data RunErr = ParseError | EvalError deriving (Eq, Show)
instance Exception RunErr

values :: [Value]
values = [ ValueNumber $ genUnsafe arbitrary
         , ValueString $ genUnsafe arbitrary
         , ValueAddress $ genUnsafe arbitrary
         , ValueBool $ genUnsafe arbitrary
         , ValueFilePath "/dev"
         , ValuePublicKey $ genUnsafe arbitrary
         , ValueStakeholderId $ genUnsafe arbitrary
         , ValueHash $ genUnsafe arbitrary
         , ValueBlockVersion $ genUnsafe arbitrary
         , ValueSoftwareVersion $ genUnsafe arbitrary
         -- to be implemented
        --  , ValueBlockVersionModifier BlockVersionModifier
         , ValueBlockVersionData $ genUnsafe arbitrary
        --  , ValueProposeUpdateSystem ProposeUpdateSystem
         , ValueAddrDistrPart $ genUnsafe arbitrary
        --  , ValueAddrStakeDistribution AddrStakeDistribution
        --  , ValueTxOut $ genUnsafe arbitrary
        --  , ValueList [Value]
         ]
  where
    genUnsafe = unsafePerformIO . generate

-- instance Generic AddrDistrPart

instance Arbitrary AddrDistrPart where
    arbitrary = genericArbitrary
    shrink = genericShrink
