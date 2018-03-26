{-# LANGUAGE RankNTypes #-}
module Test.Auxx.Printer.PrinterSpec
       ( spec
       ) where

import           Universum

import           Control.Monad.Except (ExceptT (..), withExceptT)
import           Data.Functor.Identity (Identity)
import           Lang (Arg (..), Expr (..), Lit (..), Name, ProcCall (..))
import           Lang.Syntax (AtLeastTwo (..))
import           Lang.Value (AddrDistrPart (..))
import           Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)
import           Printer (pprExpr)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Expectation, Spec, SpecWith, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, generate, property)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Text.PrettyPrint.ANSI.Leijen (Doc, text)

import qualified Lang
import qualified Test.Hspec as Hspec
import qualified Text.PrettyPrint.ANSI.Leijen as PP

spec :: Spec
spec = describe "Auxx.Repl.ppValue" $ do
    traverse_ itHandles expressions
    prop "hadles any Expr" propHandleRandomExpr

itHandles :: (Expr Name) -> SpecWith (Hspec.Arg Expectation)
itHandles val = it ("handles " <> show val) $ exprPrinter val

propHandleRandomExpr :: Property
propHandleRandomExpr = property $ ((\expr -> (parseBack_ . (pprExpr (Just 100))) expr == (Right expr)) :: Expr Name -> Bool)
  where
    parseBack_ = runIdentity . parseBack

exprPrinter :: Expr Name -> Expectation
exprPrinter expr = runIdentity $ exprPrinterId
  where
    exprPrinterId :: Identity Expectation
    exprPrinterId = do
        eithParsed <- (parseBack . (pprExpr (Just 100))) expr
        return $ eithParsed `shouldBe` (Right expr)

parseBack :: Text -> Identity (Either Doc (Expr Name))
parseBack line = withCompileInfo $(retrieveCompileTimeInfo) $ do
    let parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
    runExceptT (parse line) >>= \case
        Left errDoc ->
            let
                errMsg = ((text . toString) line PP.<$> errDoc)
            in return $ Left errMsg
        Right expr -> return $ Right expr

instance Arbitrary (Expr Name) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (AtLeastTwo (Expr Name)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ProcCall Name (Expr Name)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Lit) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Arg (Expr Name)) where
    arbitrary = genericArbitrary
    shrink = genericShrink

expressions :: [Expr Name]
expressions = [ ExprUnit
              , ExprLit (LitNumber 555)
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprLit (LitHash $ genUnsafe arbitrary)) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCall) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCallWithFunc) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCallNestedFunc) [ExprLit (LitString "Single ident")])
              , ExprLit (LitString "jjl")
              , ExprLit (LitAddress $ genUnsafe arbitrary)
              , ExprLit (LitPublicKey $ genUnsafe arbitrary)
              , ExprLit (LitHash $ genUnsafe arbitrary)
              , ExprLit (LitStakeholderId $ genUnsafe arbitrary)
              , ExprLit (LitBlockVersion $ genUnsafe arbitrary)
              , ExprLit (LitSoftwareVersion $ genUnsafe arbitrary)
              , ExprLit (LitFilePath "/kkk")]

genUnsafe :: Gen c -> c
genUnsafe = unsafePerformIO . generate

procCall, procCallWithFunc, procCallNestedFunc :: ProcCall Name (Expr Name)
procCall = ProcCall "foo-a" [ArgKw "foo-arg" (ExprLit (LitString "argValue")), (ArgPos (ExprLit (LitString "posValue"))), ArgKw "foo-a-arg-name" (ExprLit (LitString "1 idented"))]
procCallWithFunc = ProcCall "foo-b" [ArgKw "foo-b-arg-name" (ExprLit (LitString "argValue")), (ArgPos (ExprProcCall procCall)), ArgKw "foo-b-arg-name" (ExprLit (LitString "2 idented"))]
procCallNestedFunc = ProcCall "foo-c" [ArgKw "foo-c-arg-name" (ExprLit (LitString "argValue")), (ArgPos (ExprProcCall procCallWithFunc))]

instance Eq Doc

data RunErr = ParseError | EvalError deriving (Eq, Show)
instance Exception RunErr

instance Arbitrary AddrDistrPart where
    arbitrary = genericArbitrary
    shrink = genericShrink
