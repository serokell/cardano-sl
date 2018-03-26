{-# LANGUAGE RankNTypes #-}
module Test.Auxx.Printer.PrinterSpec
       ( spec
       ) where

import           Universum

import           Control.Monad.Except (ExceptT (..), withExceptT)
import           Data.Functor.Identity (Identity)
import qualified Data.Text as T
import           Formatting (build, sformat, (%))
import           Printer (pprExpr, valueToExpr)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec (Expectation, Spec, SpecWith, describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Arbitrary (..), Gen, Property, generate, property)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Command.Proc (createCommandProcs)
import           Lang (Arg (..), Expr (..), Lit (..), Name, ProcCall (..), evaluate,
                       resolveCommandProcs)
import qualified Lang
import           Lang.Syntax (AtLeastTwo (..))
import qualified Lang.Value as Lang (Value (..))
import           Pos.Util.CompileInfo (retrieveCompileTimeInfo, withCompileInfo)

spec :: Spec
spec = do
    describe "Auxx.Printer.pprExpr" $ do
        traverse_ itPrintsCorrectly expressions
        prop "handles any Expr" propHandleRandomExpr
    describe "Auxx.Printer.valueToExpr" $ do
        traverse_ itConvertsCorrectly values

propHandleRandomExpr :: Property
propHandleRandomExpr = property p
  where
    parseBack_ :: Text -> Either Doc (Expr Name)
    parseBack_ = runIdentity . parseBack

    p :: Expr Name -> Bool
    p expr =
        (parseBack_ . (pprExpr (Just 100))) expr == (Right expr)

itPrintsCorrectly :: Expr Name -> SpecWith (Hspec.Arg Expectation)
itPrintsCorrectly expr =
    it ("handles " <> show expr) $
        exprPrinter expr

itConvertsCorrectly :: Lang.Value -> SpecWith (Hspec.Arg Expectation)
itConvertsCorrectly val =
    it ("handles " <> show val) $
        valConverter val

exprPrinter :: Expr Name -> Expectation
exprPrinter expr = runIdentity exprPrinterId
  where
    exprPrinterId :: Identity Expectation
    exprPrinterId = do
        eithParsed <- (parseBack . (pprExpr (Just 100))) expr
        return $ eithParsed `shouldBe` (Right expr)

valConverter :: Lang.Value -> Expectation
valConverter val = runIdentity valConverterId
  where
    valConverterId :: Identity Expectation
    valConverterId = do
        eithVal <- evaluate $
            either namesNotFound identity $
            resolveCommandProcs commandProcs $ valueToExpr val
        return $ eithVal `shouldBe` (Right val)

    namesNotFound :: NonEmpty Name -> a
    namesNotFound names =
        error $ sformat ("Failed to resolve names: "%build) (T.intercalate ", " $ map pretty $ toList names)

    printAction _ = return ()
    commandProcs = withCompileInfo $(retrieveCompileTimeInfo) $
        createCommandProcs Nothing Nothing printAction Nothing :: [Lang.CommandProc Identity]

parseBack :: Text -> Identity (Either Doc (Expr Name))
parseBack line = withCompileInfo $(retrieveCompileTimeInfo) $ do
    let parse = withExceptT Lang.ppParseError . ExceptT . return . Lang.parse
    runExceptT (parse line) >>= \case
        Left errDoc ->
            let
                errMsg = ((PP.text . toString) line PP.<$> errDoc)
            in return $ Left errMsg
        Right expr -> return $ Right expr

instance Eq Doc where
    a == b = (show a :: Text) == (show b :: Text)

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

genUnsafe :: Gen c -> c
genUnsafe = unsafePerformIO . generate
expressions :: [Expr Name]
expressions =
    [ ExprUnit
    , ExprLit (LitNumber 555)
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprLit (LitHash $ genUnsafe arbitrary)) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCall) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCallWithFunc) [])
              , ExprGroup (AtLeastTwo (ExprLit (LitNumber 555)) (ExprProcCall procCallNestedFunc) [ExprLit (LitString "Single ident")])
    , ExprLit (LitString "jjl")
    , ExprLit (LitAddress         $ genUnsafe arbitrary)
    , ExprLit (LitPublicKey       $ genUnsafe arbitrary)
    , ExprLit (LitHash            $ genUnsafe arbitrary)
    , ExprLit (LitStakeholderId   $ genUnsafe arbitrary)
    , ExprLit (LitBlockVersion    $ genUnsafe arbitrary)
    , ExprLit (LitSoftwareVersion $ genUnsafe arbitrary)
    , ExprLit (LitFilePath "/kkk")
    ]
  where
    procCall, procCallWithFunc, procCallNestedFunc :: ProcCall Name (Expr Name)
    procCall = ProcCall "foo-a"
        [ ArgKw "foo-arg" (ExprLit (LitString "argValue"))
        , ArgPos (ExprLit (LitString "posValue"))
        , ArgKw "foo-a-arg-name" (ExprLit (LitString "1 idented"))
        ]
    procCallWithFunc = ProcCall "foo-b"
        [ ArgKw "foo-b-arg-name" (ExprLit (LitString "argValue"))
        , ArgPos (ExprProcCall procCall)
        , ArgKw "foo-b-arg-name" (ExprLit (LitString "2 idented"))
        ]
    procCallNestedFunc = ProcCall "foo-c"
        [ ArgKw "foo-c-arg-name" (ExprLit (LitString "argValue"))
        , ArgPos (ExprProcCall procCallWithFunc)
        ]

data RunErr = ParseError | EvalError deriving (Eq, Show)
instance Exception RunErr

values :: [Lang.Value]
values =
    [ Lang.ValueNumber                $ genUnsafe arbitrary
    , Lang.ValueString                $ genUnsafe arbitrary
    , Lang.ValueAddress               $ genUnsafe arbitrary
    , Lang.ValueBool                  $ genUnsafe arbitrary
    , Lang.ValueFilePath              "/dev"
    , Lang.ValuePublicKey             $ genUnsafe arbitrary
    , Lang.ValueStakeholderId         $ genUnsafe arbitrary
    , Lang.ValueHash                  $ genUnsafe arbitrary
    , Lang.ValueBlockVersion          $ genUnsafe arbitrary
    , Lang.ValueSoftwareVersion       $ genUnsafe arbitrary
    , Lang.ValueBlockVersionModifier  $ genUnsafe arbitrary
    , Lang.ValueBlockVersionData      $ genUnsafe arbitrary
    , Lang.ValueProposeUpdateSystem   $ genUnsafe arbitrary
    , Lang.ValueAddrDistrPart         $ genUnsafe arbitrary
    , Lang.ValueAddrStakeDistribution $ genUnsafe arbitrary
    , Lang.ValueTxOut                 $ genUnsafe arbitrary
    , Lang.ValueList                  $ genUnsafe arbitrary
    ]
