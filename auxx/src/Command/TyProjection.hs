module Command.TyProjection
       ( tyValue
       , tyEither
       , tyAddress
       , tyPublicKey
       , tyTxOut
       , tyAddrStakeDistr
       , tyFilePath
       , tyInt
       , tyWord
       , tyWord8
       , tyWord32
       , tyWord64
       , tyByte
       , tySecond
       , tyCoeff
       , tyBool
       , tyScriptVersion
       , tyCoin
       , tyCoinPortion
       , tyStakeholderId
       , tyAddrDistrPart
       , tyEpochIndex
       , tyHash
       , tyBlockVersion
       , tySoftwareVersion
       , tyBlockVersionModifier
       , tySoftforkRule
       , tyTxFeePolicy
       , tyProposeUpdateSystem
       , tySystemTag
       , tyApplicationName
       , tyString
       , tyByteString
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Scientific (Scientific, floatingOrInteger, toBoundedInteger,
                                  toBoundedRealFloat, toRealFloat)
import           Data.Time.Units (Microsecond, TimeUnit, convertUnit, fromMicroseconds)
import           Serokell.Data.Memory.Units (Byte, fromBytes)

import           Pos.Core (AddrStakeDistribution (..), Address, BlockVersion, Coin, CoinPortion,
                           EpochIndex, ScriptVersion, SoftwareVersion, StakeholderId, mkCoin,
                           unsafeCoinPortionFromDouble, unsafeGetCoin)
import           Pos.Core.Common (Coeff (..), TxFeePolicy)
import           Pos.Core.Txp (TxOut (..))
import           Pos.Core.Update (SoftforkRule)
import           Pos.Crypto (AHash (..), Hash, PublicKey)
import           Pos.Update (ApplicationName (..), BlockVersionModifier (..), SystemTag (..))

import           Lang.Argument (TyProjection (..), TypeName (..))
import           Lang.Value (AddrDistrPart (..), ProposeUpdateSystem (..), Value (..),
                             _ValueAddrDistrPart, _ValueAddrStakeDistribution, _ValueAddress,
                             _ValueBlockVersion, _ValueBlockVersionModifier, _ValueBool,
                             _ValueFilePath, _ValueHash, _ValueNumber, _ValueProposeUpdateSystem,
                             _ValuePublicKey, _ValueSoftforkRule, _ValueSoftwareVersion,
                             _ValueStakeholderId, _ValueString, _ValueTxFeePolicy, _ValueTxOut)

tyValue :: TyProjection Value
tyValue = TyProjection "Value" Just

infixr `tyEither`

tyEither :: TyProjection a -> TyProjection b -> TyProjection (Either a b)
tyEither tpa tpb = TyProjection
    { tpTypeName = TypeNameEither (tpTypeName tpa) (tpTypeName tpb)
    , tpMatcher = \v ->
        Left <$> tpMatcher tpa v <|>
        Right <$> tpMatcher tpb v
    }

tyAddress :: TyProjection Address
tyAddress = TyProjection "Address" (preview _ValueAddress)

tyPublicKey :: TyProjection PublicKey
tyPublicKey = TyProjection "PublicKey" (preview _ValuePublicKey)

tyTxOut :: TyProjection TxOut
tyTxOut = TyProjection "TxOut" (preview _ValueTxOut)

tyAddrStakeDistr :: TyProjection AddrStakeDistribution
tyAddrStakeDistr = TyProjection "AddrStakeDistribution" (preview _ValueAddrStakeDistribution)

tyFilePath :: TyProjection FilePath
tyFilePath = TyProjection "FilePath" (preview _ValueFilePath)

tyInt :: TyProjection Int
tyInt = TyProjection "Int" (toBoundedInteger <=< preview _ValueNumber)

tyWord :: TyProjection Word
tyWord = TyProjection "Word" (toBoundedInteger <=< preview _ValueNumber)

tyWord8 :: TyProjection Word8
tyWord8 = TyProjection "Word8" (toBoundedInteger <=< preview _ValueNumber)

tyWord32 :: TyProjection Word32
tyWord32 = TyProjection "Word32" (toBoundedInteger <=< preview _ValueNumber)

tyWord64 :: TyProjection Word64
tyWord64 = TyProjection "Word64" (toBoundedInteger <=< preview _ValueNumber)

tyByte :: TyProjection Byte
tyByte = fromBytes <$> TyProjection "Byte" (sciToInteger <=< preview _ValueNumber)

sciToInteger :: Scientific -> Maybe Integer
sciToInteger = either (const Nothing) Just . floatingOrInteger @Double @Integer

tySecond :: TimeUnit a => TyProjection a
tySecond = secToTimeUnit <$> TyProjection "Second" (toDouble <=< preview _ValueNumber)
  where
    toDouble :: Scientific -> Maybe Double
    toDouble = rightToMaybe . toBoundedRealFloat

    -- Using microseconds here because that's how time-units stores times internally.
    secToTimeUnit :: TimeUnit a => Double -> a
    secToTimeUnit = convertUnit @Microsecond . fromMicroseconds . round . (* 1e6)

tyCoeff :: TyProjection Coeff
tyCoeff = Coeff . realToFrac <$> TyProjection "Coeff" (preview _ValueNumber)

tyScriptVersion :: TyProjection ScriptVersion
tyScriptVersion = TyProjection "ScriptVersion" (toBoundedInteger <=< preview _ValueNumber)

tyBool :: TyProjection Bool
tyBool = TyProjection "Bool" (preview _ValueBool)

-- | Small hack to use 'toBoundedInteger' for 'Coin'.
newtype PreCoin = PreCoin { getPreCoin :: Word64 }
    deriving (Eq, Ord, Num, Enum, Real, Integral)

instance Bounded PreCoin where
    minBound = PreCoin . unsafeGetCoin $ minBound
    maxBound = PreCoin . unsafeGetCoin $ maxBound

fromPreCoin :: PreCoin -> Coin
fromPreCoin = mkCoin . getPreCoin

tyCoin :: TyProjection Coin
tyCoin = fromPreCoin <$> TyProjection "Coin" (toBoundedInteger <=< preview _ValueNumber)

coinPortionFromDouble :: Double -> Maybe CoinPortion
coinPortionFromDouble a
    | a >= 0, a <= 1 = Just $ unsafeCoinPortionFromDouble a
    | otherwise = Nothing

tyCoinPortion :: TyProjection CoinPortion
tyCoinPortion = TyProjection "CoinPortion" (coinPortionFromDouble . toRealFloat <=< preview _ValueNumber)

tyStakeholderId :: TyProjection StakeholderId
tyStakeholderId = TyProjection "StakeholderId" (preview _ValueStakeholderId)

tyAddrDistrPart :: TyProjection AddrDistrPart
tyAddrDistrPart = TyProjection "AddrDistrPart" (preview _ValueAddrDistrPart)

tyEpochIndex :: TyProjection EpochIndex
tyEpochIndex = TyProjection "EpochIndex" (toBoundedInteger <=< preview _ValueNumber)

tyHash :: TyProjection (Hash a)
tyHash = getAHash <$> TyProjection "Hash" (preview _ValueHash)

tyBlockVersion :: TyProjection BlockVersion
tyBlockVersion = TyProjection "BlockVersion" (preview _ValueBlockVersion)

tySoftwareVersion :: TyProjection SoftwareVersion
tySoftwareVersion = TyProjection "SoftwareVersion" (preview _ValueSoftwareVersion)

tyBlockVersionModifier :: TyProjection BlockVersionModifier
tyBlockVersionModifier = TyProjection "BlockVersionModifier" (preview _ValueBlockVersionModifier)

tySoftforkRule :: TyProjection SoftforkRule
tySoftforkRule = TyProjection "SoftforkRule" (preview _ValueSoftforkRule)

tyTxFeePolicy :: TyProjection TxFeePolicy
tyTxFeePolicy = TyProjection "TxFeePolicy" (preview _ValueTxFeePolicy)

tyProposeUpdateSystem :: TyProjection ProposeUpdateSystem
tyProposeUpdateSystem = TyProjection "ProposeUpdateSystem" (preview _ValueProposeUpdateSystem)

tySystemTag :: TyProjection SystemTag
tySystemTag = TyProjection "SystemTag" ((fmap . fmap) (SystemTag . fromString) (preview _ValueString))

tyApplicationName :: TyProjection ApplicationName
tyApplicationName = TyProjection "ApplicationName" ((fmap . fmap) (ApplicationName . fromString) (preview _ValueString))

tyString :: TyProjection String
tyString = TyProjection "String" (preview _ValueString)

tyByteString :: TyProjection ByteString
tyByteString = BS.pack <$> TyProjection "ByteString" (preview _ValueString)
