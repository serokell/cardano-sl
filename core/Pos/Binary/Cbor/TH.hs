{-# LANGUAGE ScopedTypeVariables #-}

{-
TH helpers for Bi.

Suppose you have the following datatype:

data User
    = Login {
      login :: String
    , age   :: Int
    }
    | FullName {
      firstName  :: String
    , lastName   :: String
    , sex        :: Bool
    }

then the next deriveSimpleBi:

deriveSimpleBi ''User [
    Cons 'Login [
        Field [| login :: String |],
        Field [| age   :: Int    |],
    ],
    Cons 'FullName [
        Field [| firstName :: String |],
        Field [| lastName  :: String |],
        Unused 'sex
    ]]

will generate:

instance Bi User where
    encode = \x -> case x of
        val@Login{} -> encode (0 :: Word8)
                    <> encode (login val)
                    <> encode (age val)
        val@FullName{} -> encode (1 :: Word8)
                       <> encode (firstName val)
                       <> encode (age val)
    get = label "User" $ do
        tag <- get @Word8
        case tag of
            0 -> do
                login <- get
                age <- get
                pure $ Login {..}
            1 -> do
                firstName <- get
                age <- get
                let secondName = def
                pure $ FullName {..}
            _ -> fail ("Found invalid tag while getting User")
-}

module Pos.Binary.Cbor.TH
       ( deriveSimpleBi
       , Cons (..)
       , Field (Field)
       ) where

import           Universum

import           Control.Lens          (imap)
import           Data.Default          (def)
import           Data.List             (notElem, nubBy, partition)
import qualified Data.Text             as T
import           Formatting            (sformat, shown, (%))
import           Language.Haskell.TH
import           TH.ReifySimple        (DataCon (..), DataType (..), reifyDataType)
import           TH.Utilities          (plainInstanceD)

import qualified Pos.Binary.Cbor.Class as Bi

data Cons = Cons
    { -- | Name of a constructor.
      cName   :: Name
      -- | Field of a constructor.
    , cFields :: [Field]
    }

data Field
    = Field {
    -- ^ The constructor means that you want
    -- a field to participate in serialisation/deserialization
      fFieldAndType :: ExpQ
    -- ^ You're expected to write something like @[|foo :: Bar|]@ here
    }
    | Unused {
    -- ^ The constructor means that you don't want
    -- a field to participate in serialisation/deserialization
      fName :: Name
    -- ^ Name of unused field
    }

-- | Turn something like @[|foo :: Bar|]@ into @(foo, Bar)@.
expToNameAndType :: ExpQ -> Q (Name, Type)
expToNameAndType ex = ex >>= \case
    SigE (VarE n) t -> pure (n, t)
    other           -> fail $ "expToNameAndType: the expression should look \
                              \like [|fname :: FType|], but it doesn't: "
                              <> show other

fieldToPair :: Field -> Q (Name, Maybe Type)
fieldToPair (Unused nm) = pure (nm, Nothing)
fieldToPair (Field ex)  = over _2 Just <$> expToNameAndType ex

-- Some part of code copied from
-- https://hackage.haskell.org/package/store-0.4.3.1/docs/src/Data-Store-TH-Internal.html#makeStore

-- | Takes the name of datatype and constructors of datatype and generates Bi instances.
-- You should pass all constructors explicitly. Also, you should pass all fields explicitly,
-- each of them should be @Field@ or @Unused@,
-- and the real type of field and the passed (in the Field) type should be same.
-- All field of datatype should be named explicitly.
-- The numbers of constructors must be at least one and at most 255.
-- The order of fields matter: it corresponds to order of put's and get's.
-- If some of these statements is violated,
-- you will get compile error with the corresponding message.
deriveSimpleBi :: Name -> [Cons] -> Q [Dec]
deriveSimpleBi headTy constrs = do
    when (null constrs) $
        failText "You passed no constructors to deriveSimpleBi"
    when (length constrs > 255) $
        failText "You passed too many constructors to deriveSimpleBi"
    when (length (nubBy ((==) `on` cName) constrs) /= length constrs) $
        failText "You passed two constructors with the same name"
    dt <- reifyDataType headTy
    case matchAllConstrs constrs (dtCons dt) of
        MissedCons cons ->
            failText .
                sformat ("Constructor '"%shown%"' isn't passed to deriveSimpleBi") $
                cons
        UnknownCons cons ->
            failText .
                sformat ("Unknown constructor '"%shown%"' is passed to deriveSimpleBi") $
                cons
        MatchedCons matchedConstrs ->
            forM_ (zip constrs matchedConstrs) $ \(Cons{..}, DataCon{..}) -> do
                let realFields = mapMaybe (\(n, t) -> (,t) <$> n) dcFields
                when (length realFields /= length dcFields) $
                    failText $ sformat ("Some field of "%shown
                                       %" constructor doesn't have an explicit name") cName
                cResolvedFields <- mapM fieldToPair cFields
                case checkAllFields cResolvedFields realFields of
                    MissedField field ->
                        failText $ sformat ("Field '"%shown%"' of the constructor '"
                                            %shown%"' isn't passed to deriveSimpleBi")
                                   field cName
                    UnknownField field ->
                        failText $ sformat ("Unknown field '"%shown%"' of the constructor '"
                                            %shown%"' is passed to deriveSimpleBi")
                                   field cName
                    TypeMismatched field realType passedType ->
                        failText $ sformat ("The type of '"%shown%"' of the constructor '"
                                            %shown%"' is mismatched: real type '"
                                            %shown%"', passed type '"%shown%"'")
                                   field cName realType passedType
                    MatchedFields -> pass
    ty <- conT headTy
    makeBiInstanceTH ty <$> biEncodeExpr <*> biDecodeExpr
  where
    shortNameTy :: Text
    shortNameTy = toText $ nameBase headTy
    -- Meta information about constructors --

    -- Constructor and its used fields.
    filteredConstrs :: [Cons]
    filteredConstrs = map (\Cons{..} -> Cons cName (filter isUsed cFields)) constrs

    -- Useful variables for @size@, @put@, @get@ --
    tagType :: TypeQ
    tagType = [t| Word8 |]

    -- Helpers --
    failText :: MonadFail m => T.Text -> m a
    failText = fail . toString

    isUsed :: Field -> Bool
    isUsed (Unused _) = False
    isUsed _          = True

    -- Decode definition --
    biEncodeExpr :: Q Exp
    biEncodeExpr = do
        x <- newName "x"
        lam1E (varP x) $
          caseE (varE x) $
              imap biEncodeConstr filteredConstrs

    -- Generate the following code:
    -- val@Constr{} -> encode (3 :: Word8)
    --              <> encode (field1 val)
    --              <> encode (field2 val)
    --              <> encode (field3 val)
    biEncodeConstr :: Int -> Cons -> MatchQ
    biEncodeConstr ix (Cons cName cFields) = do
        val <- newName $ if null cFields then "_" else "val"
        match (asP val (recP cName [])) (body (varE val)) []
      where
        body val = normalB $
            if length constrs >= 2 then
                mconcatE (encodeTag ix : map (encodeField val) cFields)
            else
                mconcatE (map (encodeField val) cFields)

    encodeTag :: Int -> Q Exp
    encodeTag ix = [| Bi.encode (ix :: $tagType) |]

    encodeField :: ExpQ -> Field -> Q Exp
    encodeField val Field{..} = do
        (fName, _) <- expToNameAndType fFieldAndType
        [| Bi.encode ($(varE fName) $val) |]
    encodeField _  (Unused _) = fail "Something went wrong: encode Unused field"

    -- Decode definition --
    biDecodeExpr :: Q Exp
    biDecodeExpr = case constrs of
        []     ->
            failText $ sformat ("Attempting to decode type without constructors "%shown) headTy
        [cons] ->
            (biDecodeConstr cons) -- There is one constructor
        _      -> do
            let tagName = mkName "tag"
            let getMatch ix con = match (litP (IntegerL (fromIntegral ix)))
                                        (normalB (biDecodeConstr con)) []
            let mismatchConstr =
                    match wildP (normalB
                        [| fail $ toString ("Found invalid tag while decoding " <> shortNameTy) |]) []
            doE
                [ bindS (varP tagName) [| Bi.decode |]
                , noBindS (caseE
                                (sigE (varE tagName) tagType)
                                (imap getMatch constrs ++ [mismatchConstr]))
                ]

    biDecodeConstr :: Cons -> Q Exp
    biDecodeConstr (Cons name []) = appE (varE 'pure) (conE name)
    biDecodeConstr Cons{..} = do
        let (usedFields, unusedFields) = partition isUsed cFields

        fieldNames :: [Name] <- mapM (fmap fst . fieldToPair) usedFields
        varNames   :: [Name] <- mapM (newName . nameBase) fieldNames
        varPs :: [Pat] <- mapM varP varNames
        biGets :: [Exp] <- replicateM (length varPs) [| Bi.decode |]
        bindExprs :: [Stmt] <- mapM (uncurry bindS . bimap pure pure) (zip varPs biGets)
        let recWildUsedVars = map (\(f, ex) -> (f,) <$> varE ex) $ zip fieldNames varNames
        let recWildUnusedVars = map (\f -> (fName f,) <$> [| def |]) unusedFields
        recordWildCardReturn <- noBindS $
                                appE (varE 'pure) $
                                recConE cName $
                                recWildUsedVars ++ recWildUnusedVars
        doE $ map pure bindExprs ++ [pure recordWildCardReturn]

makeBiInstanceTH :: Type -> Exp -> Exp -> [Dec]
makeBiInstanceTH ty encodeE decodeE = one $
  plainInstanceD
        [] -- empty context
        (AppT (ConT ''Bi.Bi) ty)
        [ ValD (VarP 'Bi.encode) (NormalB encodeE) []
        , ValD (VarP 'Bi.decode) (NormalB decodeE) []
        ]

data MatchConstructors
    = MatchedCons [DataCon]
    -- ^ Constructors in matched order
    | MissedCons Name
    -- ^ Some constructor aren't passed
    | UnknownCons Name
    -- ^ Passed unknown constructor

matchAllConstrs :: [Cons] -> [DataCon] -> MatchConstructors
matchAllConstrs (map cName -> passedNames) realCons@(map dcName -> realNames)
    | Just nm <- passedNames `inclusion` realNames = UnknownCons nm
    | Just nm <- realNames `inclusion` passedNames = MissedCons nm
    | otherwise =
        let ret = mapMaybe (\x -> find ((x==) . dcName) realCons) passedNames in
        if length ret /= length passedNames then
            error "Something went wrong. Matched list of constructors has different length"
        else
            MatchedCons ret
  where
    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

data MatchFields
    = MatchedFields
    -- ^ All fields are matched
    | MissedField Name
    -- ^ Some field aren't passed
    | UnknownField Name
    -- ^ Passed field with unknown name
    | TypeMismatched Name Type Type
    -- ^ Some field has mismatched type

checkAllFields :: [(Name, Maybe Type)] -> [(Name, Type)] -> MatchFields
checkAllFields passedFields realFields
    | Just nm <- map fst passedFields `inclusion` map fst realFields = UnknownField nm
    | Just nm <- map fst realFields `inclusion` map fst passedFields = MissedField nm
    | otherwise =
        let ret = mapMaybe (\x -> find ((fst x ==) . fst) realFields) passedFields in
        if length ret /= length passedFields then
            error "Something went wrong. Matched list of fields has different length"
        else
            case dropWhile checkTypes (zip realFields passedFields) of
                []                                -> MatchedFields
                (((n, real), (_, Just passed)):_) -> TypeMismatched n real passed
                (((_, _), (_, Nothing)):_)        -> error "Something went wrong: illegal mismatch type"
  where
    checkTypes :: ((Name, Type), (Name, Maybe Type)) -> Bool
    checkTypes (_, (_, Nothing))       = True
    checkTypes ((_, t1), (_, Just t2)) = t1 == t2

    inclusion :: [Name] -> [Name] -> Maybe Name
    inclusion c1 c2 = find (`notElem` c2) c1

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Put '(<>)' between expressions.
mconcatE :: [ExpQ] -> ExpQ
mconcatE = foldr (\a b -> infixApp a [| (<>) |] b) [| mempty |]
