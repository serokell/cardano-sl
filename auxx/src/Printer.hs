module Printer
    ( pprLit
    , pprExpr
    , ppExpr) where

import           Universum

import           Formatting (build, float, sformat, stext, string, (%))
import           Lang.DisplayError (nameToDoc, text)
import           Lang.Name (Name)
import           Lang.Syntax (Arg (..), AtLeastTwo (..), Expr (..), Lit (..), ProcCall (..),
                              toList_)
import           Pos.Core (ApplicationName (..), SoftwareVersion (..))
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)
import           Text.PrettyPrint.ANSI.Leijen (Doc, char, indent, parens, punctuate, vsep)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

pprLit :: Lit -> Text
pprLit = f
  where
    f          (LitNumber a) = sformat float a
    f          (LitString a) = sformat ("\""%string%"\"") a
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
    f (ExprGroup exps)  = pprGroup exps
    f (ExprProcCall pc) = pprProcCall pc
    f (ExprLit l)       = pprLit l

    pprProcCall (ProcCall name args) = (sformat build name)
        <> " "
        <> concatSpace args

    concatSpace []     = ""
    concatSpace (a:as) = (pprArg a) <> (concatSpace as)

    pprArg (ArgPos pos)     = parensIfProcCall pos
    pprArg (ArgKw name val) = (sformat build name) <> ": " <> (parensIfProcCall val)

    -- Nested procCall's should be in parentheses
    parensIfProcCall (ExprProcCall pc) = parens_ (pprProcCall pc)
    parensIfProcCall anything          = pprExprNoIdent anything

    pprGroup :: AtLeastTwo (Expr Name) -> Text
    pprGroup (AtLeastTwo x y zs) = case nonEmpty zs of
        Nothing   -> parens_ (pprTwo x y)
        (Just es) -> parens_ ((pprTwo x y) <> pprNonEmpty es)

    pprTwo :: (Expr Name) -> (Expr Name) -> Text
    pprTwo e1 e2 = (pprExprNoIdent e1) <> "; " <> (pprExprNoIdent e2)

    pprNonEmpty :: NonEmpty (Expr Name) -> Text
    pprNonEmpty (e:|es) = case nonEmpty es of
        Nothing    -> pprExprNoIdent e
        (Just es_) -> (pprExprNoIdent e) <> "; " <> pprNonEmpty es_

    parens_ :: Text -> Text
    parens_ t = sformat ("("%stext%")") t

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
    ppArg i (ArgPos pos) = parensIfProcCall i pos
    ppArg i (ArgKw name val) =
        (nameToDoc name) PP.<> (text ": ") PP.<> (parensIfProcCall i val)

    parensIfProcCall :: Indent -> Expr Name -> Doc
    parensIfProcCall i (ExprProcCall pc) = parens (ppProcCall (i + 1) pc)
    parensIfProcCall _ anything          = ppExpr anything

-- Do we need a width here?
pprExpr :: Maybe Width -> Expr Name -> Text
pprExpr Nothing  = pprExprNoIdent
pprExpr (Just _) = show . ppExpr
