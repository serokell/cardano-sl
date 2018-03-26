module Printer
    ( pprLit
    , pprExpr) where

import           Universum

-- import           Data.Text as Text
import           Formatting (build, float, sformat, stext, (%))
import           Lang.DisplayError (nameToDoc, text)
import           Lang.Name (Name)
import           Lang.Syntax (Arg (..), AtLeastTwo (..), Expr (..), Lit (..), ProcCall (..),
                              toList_)
import           Pos.Core (ApplicationName (..), SoftwareVersion (..))
-- import           Pos.Core.Common (CoinPortion (..))
-- import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)
import           Text.PrettyPrint.ANSI.Leijen (Doc, char, indent, parens, punctuate, vsep)

-- import qualified Data.List.NonEmpty as NE
import qualified Text.PrettyPrint.ANSI.Leijen as PP

pprLit :: Lit -> Text
pprLit = f
  where
    f          (LitNumber a) = sformat float a
    f          (LitString a) = sformat ("\""%stext%"\"") (toText a)
    f         (LitAddress a) = pretty a
    f       (LitPublicKey a) = sformat fullPublicKeyF a
    f   (LitStakeholderId a) = sformat hashHexF a
    f            (LitHash a) = sformat hashHexF (getAHash a)
    f    (LitBlockVersion a) = pretty a
    f (LitSoftwareVersion a) = printSoftware a
    f        (LitFilePath a) = pretty a

    printSoftware :: SoftwareVersion -> Text
    printSoftware SoftwareVersion {..} =
        sformat ("software name: \""%stext%"\" n: "%build) (getApplicationName svAppName) svNumber

pprExprNoIdent :: Expr Name -> Text
pprExprNoIdent = f
  where
    f ExprUnit          = sformat (stext) "()"
    f (ExprGroup exps)  = ppGroup exps
    f (ExprProcCall pc) = ppProcCall pc
    f (ExprLit l)       = pprLit l

    ppProcCall (ProcCall name args) = (sformat build name)
        <> " "
        <> concatSpace args

    concatSpace []     = ""
    concatSpace [arg]  = ppArg arg -- redundant
    concatSpace (a:as) = (ppArg a) <> (concatSpace as)

    ppArg (ArgPos pos)     = pprExprNoIdentNested pos
    ppArg (ArgKw name val) = (sformat build name) <> ": " <> (pprExprNoIdent val)

    -- Nested procCall's should be in parentheses
    pprExprNoIdentNested (ExprProcCall pc) = "(" <> ppProcCall pc <> ")"
    pprExprNoIdentNested anything          = pprExprNoIdentNested anything

    ppGroup :: AtLeastTwo (Expr Name) -> Text
    ppGroup (AtLeastTwo x y zs) = case nonEmpty zs of
        Nothing   -> ppTwo x y
        (Just es) -> (ppTwo x y) <> ppNonEmpty es

    ppTwo :: (Expr Name) -> (Expr Name) -> Text
    ppTwo e1 e2 = (pprExprNoIdent e1) <> "; " <> (pprExprNoIdent e2)

    ppNonEmpty :: NonEmpty (Expr Name) -> Text
    ppNonEmpty (e:|es) = case nonEmpty es of
        Nothing    -> pprExprNoIdent e
        (Just es_) -> (pprExprNoIdent e) <> "; " <> ppNonEmpty es_

type Indent = Int
type Width = Int

ppExpr :: Expr Name -> Doc
ppExpr = f
  where
    f ExprUnit          = text "()"
    f (ExprGroup exps)  = ppGroup exps
    f (ExprProcCall pc) = ppProcCall 2 pc
    f (ExprLit l)       = text (pprLit l)

    ppGroup :: AtLeastTwo (Expr Name) -> Doc
    ppGroup expsALT = parens $ vsep (punctuate (char ';') (fmap ppExpr (toList_ expsALT)))

    ppProcCall :: Indent -> (ProcCall Name (Expr Name)) -> Doc
    ppProcCall i (ProcCall name args) =
        let
            nameDoc = nameToDoc name
            argsDoc = indent i (vsep (fmap (ppArg i) args))
        in
            nameDoc PP.<$> argsDoc

    ppArg :: Indent -> Arg (Expr Name) -> Doc
    ppArg i (ArgPos pos) = case pos of
        -- pass (i + 1) to increase nesting if arg is a ProcCall
        (ExprProcCall pc) -> parens (ppProcCall (i + 1) pc)
        anything          -> ppExpr anything
    ppArg _ (ArgKw name val) = (nameToDoc name) PP.<> (text ": ") PP.<> (ppExpr val)


-- formatTest :: IO ()
-- formatTest = do
--     putText $ pprExpr (Just 100) $ ExprGroup $ fromList_ [(ExprLit (LitNumber 555)), ExprProcCall procCallNestedFunc]

-- procCall = ProcCall "foo-a" [ArgKw "foo-arg" (ExprLit (LitString "argValue")), (ArgPos (ExprLit (LitString "posValue"))), ArgKw "foo-a-arg-name" (ExprLit (LitString "1 idented"))]
-- procCallWithFunc = ProcCall "foo-b" [ArgKw "foo-b-arg-name" (ExprLit (LitString "argValue")), (ArgPos (ExprProcCall procCall)), ArgKw "foo-b-arg-name" (ExprLit (LitString "2 idented"))]
-- procCallNestedFunc = ProcCall "foo-c" [ArgKw "foo-c-arg-name" (ExprLit (LitString "argValue")), (ArgPos (ExprProcCall procCallWithFunc))]


pprExpr :: Maybe Width -> Expr Name -> Text
pprExpr Nothing      = pprExprNoIdent
pprExpr (Just width) = show . ppExpr
