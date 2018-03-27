module Printer
    ( pprLit
    , pprExpr
    , ppExpr
    , pprValue
    , valueToExpr
    ) where

import           Universum

import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import           Data.Fixed (Nano)
import qualified Data.Map as M (toList)
import           Data.Scientific (Scientific)
import           Data.Time.Units (Millisecond)
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
import           Pos.Core.Common (AddrStakeDistribution (..), Coeff (..), Coin (..),
                                  CoinPortion (..), TxFeePolicy (..), TxSizeLinear (..),
                                  coinPortionDenominator)
import           Pos.Core.Txp (TxOut (..))
import           Pos.Core.Update (BlockVersionData (..), BlockVersionModifier (..),
                                  SoftforkRule (..), SystemTag (..))
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
    f ExprUnit          = sformat (stext) "()"
    f (ExprGroup exps)  = pprGroup exps
    f (ExprProcCall pc) = pprProcCall pc
    f (ExprLit l)       = pprLit l

    pprProcCall (ProcCall name args) =
        sformat (build%" "%build) name (concatSpace args)

    concatSpace = foldMap pprArg

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

pprValue :: Maybe Width -> Lang.Value -> Text
pprValue mWidth = pprExpr mWidth . valueToExpr

ppExpr :: Expr Name -> Doc
ppExpr = f
  where
    f ExprUnit          = text "()"
    f (ExprGroup exps)  = ppGroup exps
    f (ExprProcCall pc) = ppProcCall 2 pc
    f (ExprLit l)       = text (pprLit l)

    ppGroup :: AtLeastTwo (Expr Name) -> Doc
    ppGroup expsALT =
        PP.parens $ PP.vsep (PP.punctuate (PP.char ';') (fmap ppExpr (toList_ expsALT)))

    ppProcCall :: Indent -> (ProcCall Name (Expr Name)) -> Doc
    ppProcCall i (ProcCall name args) =
        let
            nameDoc = nameToDoc name
            argsDoc = PP.indent i (PP.vsep (fmap (ppArg i) args))
        in
            nameDoc PP.<$> argsDoc

    ppArg :: Indent -> Arg (Expr Name) -> Doc
    ppArg i (ArgPos pos) = parensIfProcCall i pos
    ppArg i (ArgKw name val) =
        (nameToDoc name) PP.<> (text ": ") PP.<> (parensIfProcCall i val)

    parensIfProcCall :: Indent -> Expr Name -> Doc
    parensIfProcCall i (ExprProcCall pc) = PP.parens (ppProcCall (i + 1) pc)
    parensIfProcCall _ anything          = ppExpr anything

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
    Lang.ValueSoftforkRule sr           -> ExprProcCall $ ProcCall "softfork-rule" $ srToArgs sr
    Lang.ValueTxFeePolicy tfp           -> ExprProcCall $ tfpToProcCall tfp
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

    srToArgs SoftforkRule {..} =
        [ ArgPos (ExprLit $ LitNumber $ coinPortionToScientific srInitThd)
        , ArgPos (ExprLit $ LitNumber $ coinPortionToScientific srMinThd)
        , ArgPos (ExprLit $ LitNumber $ coinPortionToScientific srThdDecrement)
        ]

    tfpToProcCall = \case
        TxFeePolicyTxSizeLinear (TxSizeLinear a b) ->
            ProcCall "tx-fee-policy-tx-size-linear"
                [ ArgPos (ExprLit $ LitNumber $ realToFrac @Nano @Scientific $ coerce a)
                , ArgPos (ExprLit $ LitNumber $ realToFrac @Nano @Scientific $ coerce b)
                ]
        TxFeePolicyUnknown v bs ->
            ProcCall "tx-fee-policy-unknown"
                [ ArgPos (ExprLit $ LitNumber $ fromIntegral v)
                , ArgPos (ExprLit $ LitString $ BS.unpack bs )
                ]

    millisecToSec :: Millisecond -> Scientific
    millisecToSec = (/ 1e3) . fromIntegral

    bvmToArgs :: BlockVersionModifier -> [Arg (Expr Name)]
    bvmToArgs BlockVersionModifier {..} = catMaybes
        [ ArgKw "script-version"      . ExprLit . LitNumber . fromIntegral <$> bvmScriptVersion
        , ArgKw "slot-duration"       . ExprLit . LitNumber . millisecToSec <$> bvmSlotDuration
        , ArgKw "max-block-size"      . ExprLit . LitNumber . fromIntegral <$> bvmMaxBlockSize
        , ArgKw "max-header-size"     . ExprLit . LitNumber . fromIntegral <$> bvmMaxHeaderSize
        , ArgKw "max-tx-size"         . ExprLit . LitNumber . fromIntegral <$> bvmMaxTxSize
        , ArgKw "max-proposal-size"   . ExprLit . LitNumber . fromIntegral <$> bvmMaxProposalSize
        , ArgKw "mpc-thd"             . ExprLit . LitNumber . coinPortionToScientific <$> bvmMpcThd
        , ArgKw "heavy-del-thd"       . ExprLit . LitNumber . coinPortionToScientific <$> bvmHeavyDelThd
        , ArgKw "update-vote-thd"     . ExprLit . LitNumber . coinPortionToScientific <$> bvmUpdateVoteThd
        , ArgKw "update-proposal-thd" . ExprLit . LitNumber . coinPortionToScientific <$> bvmUpdateProposalThd
        , ArgKw "update-implicit"     . ExprLit . LitNumber . fromIntegral <$> bvmUpdateImplicit
        , ArgKw "softfork-rule"       . ExprProcCall . ProcCall "softfork-rule" . srToArgs <$> bvmSoftforkRule
        , ArgKw "tx-fee-policy"       . ExprProcCall . tfpToProcCall <$> bvmTxFeePolicy
        , ArgKw "unlock-stake-epoch"  . ExprLit . LitNumber . fromIntegral <$> bvmUnlockStakeEpoch
        ]

    bvdToArgs :: BlockVersionData -> [Arg (Expr Name)]
    bvdToArgs BlockVersionData {..} =
        [ ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdScriptVersion
        , ArgPos $ ExprLit $ LitNumber $ millisecToSec bvdSlotDuration
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdMaxBlockSize
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdMaxHeaderSize
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdMaxTxSize
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdMaxProposalSize
        , ArgPos $ ExprLit $ LitNumber $ coinPortionToScientific bvdMpcThd
        , ArgPos $ ExprLit $ LitNumber $ coinPortionToScientific bvdHeavyDelThd
        , ArgPos $ ExprLit $ LitNumber $ coinPortionToScientific bvdUpdateVoteThd
        , ArgPos $ ExprLit $ LitNumber $ coinPortionToScientific bvdUpdateProposalThd
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdUpdateImplicit
        , ArgPos $ ExprProcCall $ ProcCall "softfork-rule" $ srToArgs bvdSoftforkRule
        , ArgPos $ ExprProcCall $ tfpToProcCall bvdTxFeePolicy
        , ArgPos $ ExprLit $ LitNumber $ fromIntegral bvdUnlockStakeEpoch
        ]

    pusToArgs :: Lang.ProposeUpdateSystem -> [Arg (Expr Name)]
    pusToArgs Lang.ProposeUpdateSystem {..} = catMaybes
        [ Just $ ArgPos $ ExprLit $ LitString $ toString $ getSystemTag pusSystemTag
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
