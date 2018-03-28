module Printer
    ( pprLit
    , pprExpr
    , ppExpr
    , pprValue
    , valueToExpr
    ) where

import           Universum

import qualified Data.Map as M (toList)
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import           Data.Time.Units (Second, convertUnit)
import           Formatting (build, float, sformat, stext, string, (%))
import           Lang.DisplayError (nameToDoc, text)
import           Prelude (ShowS)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           Lang.Name (Name)
import           Lang.Syntax (Arg (..), AtLeastTwo (..), Expr (..), Lit (..), ProcCall (..),
                              toList_)
import qualified Lang.Value as Lang
import           Pos.Core (ApplicationName (..), SoftwareVersion (..))
import           Pos.Core.Common (AddrStakeDistribution (..), Coin (..), CoinPortion (..),
                                  coinPortionDenominator)
import           Pos.Core.Txp (TxOut (..))
import           Pos.Core.Update (BlockVersionData, BlockVersionModifier (..))
import           Pos.Crypto (AHash (..), fullPublicKeyF, hashHexF)

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

type Indent = Int
type Width = Int

pprExpr :: Maybe Width -> Expr Name -> Text
pprExpr Nothing      = pprExprNoIdent
pprExpr (Just width) = showSToText . PP.displayS . PP.renderPretty 0.4 width . ppExpr
  where
    showSToText :: ShowS -> Text
    showSToText s = toText (s "")

pprExprNoIdent :: Expr Name -> Text
pprExprNoIdent = f
  where
    -- Outermost group do not need parens
    f (ExprGroup exps) = pprGroupNoParens exps
    f e                = pprExprNoIdentInner e

    pprExprNoIdentInner ExprUnit          = sformat (stext) "()"
    pprExprNoIdentInner (ExprGroup exps)  = pprGroup exps
    pprExprNoIdentInner (ExprProcCall pc) = pprProcCall pc
    pprExprNoIdentInner (ExprLit l)       = pprLit l

    pprProcCall (ProcCall name args) = if null args
        then sformat build name
        else sformat (build%" "%build) name (T.intercalate " " $ fmap pprArg args)

    pprArg (ArgPos pos)     = parensIfProcCall pos
    pprArg (ArgKw name val) = (sformat build name) <> ": " <> (parensIfProcCall val)

    -- Nested procCall's should be in parentheses
    parensIfProcCall (ExprProcCall pc) = parens_ (pprProcCall pc)
    parensIfProcCall anything          = pprExprNoIdentInner anything

    pprGroupNoParens :: AtLeastTwo (Expr Name) -> Text
    pprGroupNoParens = (T.intercalate "; ") . (fmap pprExprNoIdentInner) . toList_

    pprGroup :: AtLeastTwo (Expr Name) -> Text
    pprGroup = parens_ . pprGroupNoParens

    parens_ :: Text -> Text
    parens_ t = sformat ("("%stext%")") t

pprValue :: Maybe Width -> Lang.Value -> Text
pprValue mWidth = pprExpr mWidth . valueToExpr

ppExpr :: Expr Name -> Doc
ppExpr = f
  where
    -- Outermost group do not need parens
    f (ExprGroup exps) = ppGroupNoParens exps
    f e                = ppExprInner e

    ppExprInner ExprUnit          = text "()"
    ppExprInner (ExprGroup exps)  = ppGroup exps
    ppExprInner (ExprProcCall pc) = ppProcCall 1 pc
    ppExprInner (ExprLit l)       = text (pprLit l)

    ppGroup :: AtLeastTwo (Expr Name) -> Doc
    ppGroup = PP.parens . ppGroupNoParens

    ppGroupNoParens :: AtLeastTwo (Expr Name) -> Doc
    ppGroupNoParens expsALT =
        PP.vsep (PP.punctuate (PP.char ';') (fmap ppExprInner (toList_ expsALT)))

    ppProcCall :: Indent -> (ProcCall Name (Expr Name)) -> Doc
    ppProcCall i (ProcCall name args) =
        let
            nameDoc = (PP.blue . nameToDoc) name
            argsDoc = PP.indent i (PP.vsep (fmap (ppArg i) args))
        in if null args
            then nameDoc
            else nameDoc PP.<$> argsDoc

    ppArg :: Indent -> Arg (Expr Name) -> Doc
    ppArg i (ArgPos pos) = parensIfProcCall i pos
    ppArg i (ArgKw name val) =
        (nameToDoc name) PP.<> (text ": ") PP.<> (parensIfProcCall i val)

    parensIfProcCall :: Indent -> Expr Name -> Doc
    parensIfProcCall i (ExprProcCall pc) = PP.parens (ppProcCall (i + 1) pc)
    parensIfProcCall _ anything          = ppExprInner anything

valueToExpr :: Lang.Value -> Expr Name
valueToExpr = \case
    Lang.ValueUnit                      -> ExprUnit
    Lang.ValueNumber n                  -> ExprLit $ LitNumber n
    Lang.ValueString s                  -> ExprLit $ LitString s
    Lang.ValueBool b                    -> ExprProcCall $ ProcCall (boolToProcName b) []
    Lang.ValueAddress a                 -> ExprLit $ LitAddress a
    Lang.ValuePublicKey pk              -> ExprLit $ LitPublicKey pk
    Lang.ValueTxOut txOut               -> ExprProcCall $ ProcCall "tx-out" $ txOutToArgs txOut
    Lang.ValueStakeholderId sId         -> ExprLit $ LitStakeholderId sId
    Lang.ValueHash h                    -> ExprLit $ LitHash h
    Lang.ValueBlockVersion v            -> ExprLit $ LitBlockVersion v
    Lang.ValueSoftwareVersion v         -> ExprLit $ LitSoftwareVersion v
    Lang.ValueBlockVersionModifier bvm  -> ExprProcCall $ ProcCall "bvm" $ bvmToArgs bvm
    Lang.ValueBlockVersionData bvd      -> ExprProcCall $ ProcCall "bvd-read" $ bvdToArgs bvd
    Lang.ValueProposeUpdateSystem pus   -> ExprProcCall $ ProcCall "upd-bin" $ pusToArgs pus
    Lang.ValueAddrDistrPart adp         -> ExprProcCall $ ProcCall "dp" $ adpToArgs adp
    Lang.ValueAddrStakeDistribution asd -> ExprProcCall $ asdToProcCall asd
    Lang.ValueFilePath s                -> ExprLit $ LitFilePath s
    Lang.ValueList vs                   -> ExprProcCall $ ProcCall "L" $
                                               map (ArgPos . valueToExpr) vs
  where
    boolToProcName :: Bool -> Name
    boolToProcName = \case
        True  -> "true"
        False -> "false"
    txOutToArgs :: TxOut -> [Arg (Expr Name)]
    txOutToArgs TxOut {..} =
        [ ArgPos (ExprLit $ LitAddress txOutAddress)
        , ArgPos (ExprLit $ LitNumber $ fromIntegral $ getCoin txOutValue)
        ]
    coinPortionToScientific :: CoinPortion -> Scientific
    coinPortionToScientific (getCoinPortion -> num) =
        (fromIntegral num) / (fromIntegral coinPortionDenominator)
    bvmToArgs :: BlockVersionModifier -> [Arg (Expr Name)]
    bvmToArgs BlockVersionModifier {..} = catMaybes
        [ ArgKw "script-version"      . ExprLit . LitNumber . fromIntegral <$> bvmScriptVersion
        , ArgKw "slot-duration"       . ExprLit . LitNumber . fromIntegral . toSec <$> bvmSlotDuration
        , ArgKw "max-block-size"      . ExprLit . LitNumber . fromIntegral <$> bvmMaxBlockSize
        , ArgKw "max-header-size"     . ExprLit . LitNumber . fromIntegral <$> bvmMaxHeaderSize
        , ArgKw "max-tx-size"         . ExprLit . LitNumber . fromIntegral <$> bvmMaxTxSize
        , ArgKw "max-proposal-size"   . ExprLit . LitNumber . fromIntegral <$> bvmMaxProposalSize
        , ArgKw "mpc-thd"             . ExprLit . LitNumber . coinPortionToScientific <$> bvmMpcThd
        , ArgKw "heavy-del-thd"       . ExprLit . LitNumber . coinPortionToScientific <$> bvmHeavyDelThd
        , ArgKw "update-vote-thd"     . ExprLit . LitNumber . coinPortionToScientific <$> bvmUpdateVoteThd
        , ArgKw "update-proposal-thd" . ExprLit . LitNumber . coinPortionToScientific <$> bvmUpdateProposalThd
        -- (see Proc.hs)
        -- TODO bvmUpdateImplicit
        -- TODO bvmSoftforkRule
        -- TODO bvmTxFeePolicy
        , ArgKw "unlock-stake-epoch"  . ExprLit . LitNumber . fromIntegral <$> bvmUnlockStakeEpoch
        ]
      where
        toSec = convertUnit @_ @Second
    bvdToArgs :: BlockVersionData -> [Arg (Expr Name)]
    bvdToArgs bvd = [ArgPos $ ExprLit $ LitString $ show bvd]

    pusToArgs :: Lang.ProposeUpdateSystem -> [Arg (Expr Name)]
    pusToArgs Lang.ProposeUpdateSystem {..} = catMaybes
        [ Just $ ArgPos $ ExprLit $ LitString $ show pusSystemTag
        , ArgKw "installer-path" . ExprLit . LitFilePath <$> pusInstallerPath
        , ArgKw "bin-diff-path"  . ExprLit . LitFilePath <$> pusBinDiffPath
        ]

    adpToArgs :: Lang.AddrDistrPart -> [Arg (Expr Name)]
    adpToArgs Lang.AddrDistrPart {..} =
        [ ArgPos (ExprLit $ LitStakeholderId adpStakeholderId)
        , ArgPos (ExprLit $ LitNumber $ coinPortionToScientific adpCoinPortion)
        ]

    asdToProcCall :: AddrStakeDistribution -> ProcCall Name (Expr Name)
    asdToProcCall = \case
        BootstrapEraDistr -> ProcCall "boot" []
        SingleKeyDistr sId ->
            ProcCall "distr" [ArgPos $ adpToExpr $ Lang.AddrDistrPart sId maxBound]
        UnsafeMultiKeyDistr mkd ->
            ProcCall "distr" $
                map (\(sId, cp) -> ArgPos $ adpToExpr $ Lang.AddrDistrPart sId cp) $
                    M.toList mkd
      where
        adpToExpr :: Lang.AddrDistrPart -> Expr Name
        adpToExpr = valueToExpr . Lang.ValueAddrDistrPart
