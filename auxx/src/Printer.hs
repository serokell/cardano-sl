module Printer
    ( pprLit
    , pprExpr) where

import           Universum

import           Data.Text as Text
import           Formatting (build, char, float, int, sformat, stext, (%))
import           Lang.Name (Name)
import           Lang.Syntax (Arg (..), Expr (..), Lit (..), ProcCall (..))
import           Pos.Core (AddrStakeDistribution, Address, ApplicationName (..), BlockVersion,
                           CoinPortion, SoftwareVersion (..), StakeholderId)
import           Pos.Core.Common (CoinPortion (..))
import           Pos.Core.Update (BlockVersionData)
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)

import qualified Lang

pprLit :: Lit -> Text
pprLit = f
  where
    f          (LitNumber a) = (sformat float a)
    f          (LitString a) = sformat (char % stext % char) '\"' (toText a) '\"'
    f         (LitAddress a) = (pretty a)
    f       (LitPublicKey a) = (sformat fullPublicKeyF a)
    f   (LitStakeholderId a) = (sformat hashHexF a)
    f            (LitHash a) = (sformat hashHexF (getAHash a))
    f    (LitBlockVersion a) = (pretty a)
    f (LitSoftwareVersion a) = "~software~" <> pretty a
    f        (LitFilePath a) = (pretty a)


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

    ppGroup :: NonEmpty (Expr Name) -> Text
    -- ppGroup (AtLeastTwo a1 a2 as) = case as of
    --     [] -> (pprExprNoIdent a1) <> "; " <> (pprExprNoIdent a2)
    --   where
    --     ppLast [] = ""
    --     ppLast (x:xs)
    ppGroup (e:|es) = case (nonEmpty es) of
        Nothing  -> pprExprNoIdent e
        Just es_ -> (pprExprNoIdent e) <> "; " <> (ppGroup es_)


pprExprIdent :: Width -> Expr Name -> Text
pprExprIdent w = f w
  where
    -- ignoring w here
    f w ExprUnit          = sformat (stext) "()"
    f w (ExprGroup exps)  = ppGroup exps
    f w (ExprProcCall pc) = ppProcCall 0 pc
    f w (ExprLit l)       = pprLit l

    ppProcCall :: Int -> (ProcCall Name (Expr Name)) -> Text
    ppProcCall ident (ProcCall name args) = sformat (build%"\n"%stext) name (concatArgs (ident + 1) args)

    concatArgs _ []     = sformat ("\n")
    -- concatArgs ident [arg]  = ppArg ident arg -- redundant
    concatArgs ident (a:as) =
        sformat (stext%stext%stext) (identation ident) (ppArg ident a) (concatArgs ident as)
    -- identation is passed because if argument is a ProcCall, it should be increased
    ppArg ident (ArgPos pos) = case pos of
        (ExprProcCall pc) -> sformat ("("%stext%stext%")\n") (ppProcCall ident pc) (identation ident) -- remove last \n and put ")"
        anything          -> sformat (stext%"\n") (pprExprIdent 100 anything)
    ppArg _ (ArgKw name val) = sformat (build%": "%stext%"\n") name (pprExprIdent 100 val) -- need to pass term width here

    -- Nested procCall's should be in parentheses
    -- pprExprIdentNested ident (ExprProcCall pc) = "(" <> ppProcCall ident pc <> ")"
    -- pprExprIdentNested _ anything              = pprExprIdent 100 anything
    identation :: Int -> Text
    identation n = Text.replicate n (Text.pack "  ")

    ppGroup :: NonEmpty (Expr Name) -> Text -- pass ident here?
    -- ppGroup (AtLeastTwo a1 a2 as) = case as of
    --     [] -> (pprExprIdent a1) <> "; " <> (pprExprIdent a2)
    --   where
    --     ppLast [] = ""
    --     ppLast (x:xs)
    ppGroup (e:|es) = case (nonEmpty es) of
        Nothing  -> sformat (stext%"\n") (pprExprIdent 100 e)
        Just es_ ->
            let
                printedExpr = (pprExprIdent 100 e)
            in
                if Text.last printedExpr == '\n'
                    then sformat (stext%";\n"%stext) (Text.init printedExpr) (ppGroup es_)
                    else sformat (stext%";\n"%stext) printedExpr (ppGroup es_)

type Width = Int

pprExpr :: Maybe Width -> Expr Name -> Text
pprExpr Nothing      = pprExprNoIdent
pprExpr (Just width) = pprExprIdent width


-- ppValue :: Lang.Value -> Text
-- ppValue = \case
--     Lang.ValueUnit -> ""
--     Lang.ValueNumber n -> (sformat float n)
--     Lang.ValueString s -> sformat (char % stext % char) '\"' (toText s) '\"'
--     Lang.ValueBool b -> printBool b
--     Lang.ValueAddress a ->  (pretty a)
--     Lang.ValuePublicKey pk ->  (sformat fullPublicKeyF pk)
--     Lang.ValueTxOut txOut -> (pretty txOut)
--     Lang.ValueStakeholderId sId ->  (sformat hashHexF sId)
--     Lang.ValueHash h ->  (sformat hashHexF (getAHash h))
--     Lang.ValueBlockVersion v ->  (pretty v)
--     Lang.ValueSoftwareVersion v -> printSoftware v
--     Lang.ValueBlockVersionModifier bvm ->  (pretty bvm)
--     Lang.ValueBlockVersionData bvd ->  printBVD bvd
--     Lang.ValueProposeUpdateSystem pus ->  (show pus)
--     Lang.ValueAddrDistrPart adp ->  printAddrDistrPart adp
--     Lang.ValueAddrStakeDistribution asd ->  (pretty asd)
--     Lang.ValueFilePath s ->  (toText s)
--     Lang.ValueList vs -> foldMap ((mappend "  ") . ppValue) vs

-- need to implement printCommand with polymorphic input:
-- BlockVersionModifier or
-- BlockVersionData or
-- ProposeUpdateSystem or
-- AddrDistrPart or
-- AddrStakeDistribution or
-- TxOut -> Text
printBool :: Bool -> Text
printBool True  = "true"
printBool False = "false"

printSoftware :: SoftwareVersion -> Text
printSoftware SoftwareVersion {..} =
    sformat ("software name: "%char%stext%char%" n: "%build) '\"' (getApplicationName svAppName) '\"' svNumber

printAddrDistrPart :: Lang.AddrDistrPart -> Text
printAddrDistrPart (Lang.AddrDistrPart sId cp) =
    sformat ("dp s: "%char%stext%char%" p: "%build) '\"' (sformat hashHexF sId) '\"' (getCoinPortion cp)

printBVD :: BlockVersionData -> Text
printBVD bvd = sformat ("bvd-read value: "%char%stext%char) '\"' (show bvd) '\"'
