module Printer
    ( pprLit
    , pprExpr) where

import           Universum

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


pprExpr :: Expr Name -> Text
pprExpr = f
  where
    f ExprUnit          = sformat (stext) "()"
    f (ExprGroup exps)  = ppGroup exps
    f (ExprProcCall pc) = ppProcCall pc
    f (ExprLit l)       = pprLit l

    ppProcCall (ProcCall name args) = (sformat build name)
        <> " "
        <> foldMap ((<> " ") . ppArg) args

    ppArg (ArgPos pos)     = pprExprNested pos
    ppArg (ArgKw name val) = (sformat build name) <> ": " <> (pprExpr val)

    -- Nested procCall's should be in parentheses
    pprExprNested (ExprProcCall pc) = "(" <> ppProcCall pc <> ")"
    pprExprNested anything          = pprExpr anything

    ppGroup :: NonEmpty (Expr Name) -> Text
    ppGroup (e:|es) = case (nonEmpty es) of
        Nothing  -> pprExpr e
        Just es_ -> (pprExpr e) <> "; " <> (ppGroup es_)

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
