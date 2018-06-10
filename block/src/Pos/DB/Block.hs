{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface and implementation of Blocks DB: storing blocks in files on disk.

module Pos.DB.Block
       ( getBlock
       , getUndo
       , getBlund
       , putBlunds
       , deleteBlock

       , getTipBlock

       , prepareBlockDB

       -- * Pure implementation
       , dbGetSerBlockPureDefault
       , dbGetSerUndoPureDefault
       , dbPutSerBlundsPureDefault

       -- * Rocks implementation
       , dbGetSerBlockRealDefault
       , dbGetSerUndoRealDefault
       , dbPutSerBlundsRealDefault

       -- * DBSum implementation
       , dbGetSerBlockSumDefault
       , dbGetSerUndoSumDefault
       , dbPutSerBlundsSumDefault
       ) where

import           Universum

import           Control.Lens (at)
import           Data.Default (Default (def))
import qualified Database.RocksDB as Rocks
import           Formatting (sformat)

import           Pos.Binary.Block.Types ()
import           Pos.Binary.Class (decodeFull', serialize')
import           Pos.Binary.Core ()
import           Pos.Block.BHelpers ()
import           Pos.Block.Types (Blund, SlogUndo (..), Undo (..))
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import           Pos.Core.Block (Block, GenesisBlock)
import qualified Pos.Core.Block as CB
import           Pos.Crypto (hashHexF)
import           Pos.DB.BlockIndex (deleteHeaderIndex, putHeadersIndex)
import           Pos.DB.Class (MonadDB (..), MonadDBRead (..), Serialized (..), SerializedBlock,
                               SerializedBlund, SerializedUndo, getBlock, getDeserialized)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.GState.Common (getTipSomething)
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure, pureBlocksStorage)
import           Pos.DB.Rocks (DB (..), MonadRealDB, getBlockDataDB)
import           Pos.DB.Sum (MonadDBSum, eitherDB)
import           Pos.Delegation.Types (DlgUndo (..))
import           Pos.Util.Util (HasLens (..), eitherToThrow)

----------------------------------------------------------------------------
-- BlockDB related methods
----------------------------------------------------------------------------

getUndo :: MonadDBRead m => HeaderHash -> m (Maybe Undo)
getUndo = getDeserialized dbGetSerUndo

-- | Convenient wrapper which combines 'dbGetBlock' and 'dbGetUndo' to
-- read 'Blund'.
--
-- TODO Rewrite to use a single call
getBlund :: MonadDBRead m => HeaderHash -> m (Maybe (Block, Undo))
getBlund x =
    runMaybeT $
    (,) <$> MaybeT (getBlock x)
        <*> MaybeT (getUndo x)

-- | Store blunds into a single file.
--
--   Notice that this uses an unusual encoding, in order to be able to fetch
--   either the block or the undo independently without re-encoding.
putBlunds :: MonadDB m => NonEmpty Blund -> m ()
putBlunds = dbPutSerBlunds
          . map (\bu@(b,_) -> ( CB.getBlockHeader b
                              , Serialized . serialize' $ bimap serialize' serialize' bu)
                )

-- | Get 'Block' corresponding to tip.
getTipBlock :: MonadDBRead m => m Block
getTipBlock = getTipSomething "block" getBlock

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get serialization of a block with given hash from Block DB.
getSerializedBlock
    :: forall ctx m. (MonadRealDB ctx m)
    => HeaderHash -> m (Maybe ByteString)
getSerializedBlock hh = do
    DB{..} <- getBlockDataDB
    liftIO $ Rocks.get rocksDB rocksReadOpts (blockKeyMod . sformat hashHexF $ hh)

-- Get serialization of an undo data for block with given hash from Block DB.
getSerializedUndo :: (MonadRealDB ctx m) => HeaderHash -> m (Maybe ByteString)
getSerializedUndo hh = do
    DB{..} <- getBlockDataDB
    liftIO $ Rocks.get rocksDB rocksReadOpts (undoKeyMod . sformat hashHexF $ hh)

-- For every blund, put given block, its metadata and Undo data into
-- Block DB. This function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putSerializedBlunds
    :: (MonadRealDB ctx m, MonadDB m)
    => NonEmpty (CB.BlockHeader, SerializedBlund) -> m ()
putSerializedBlunds (toList -> bs) = do
    let allData = map (\(b,u) -> let hh = sformat hashHexF (headerHash b)
                                 in ( serialize' b
                                    , unSerialized u
                                    , hh
                                    )
                      )
                      bs
    DB{..} <- getBlockDataDB
    forM_ allData $ \(blk,serUndo,hh) -> liftIO $ do
        Rocks.put rocksDB rocksWriteOpts (blockKeyMod hh) blk
        Rocks.put rocksDB rocksWriteOpts (undoKeyMod hh) serUndo
    putHeadersIndex $ toList $ map (CB.getBlockHeader . fst) bs

deleteBlock :: (MonadRealDB ctx m, MonadDB m) => HeaderHash -> m ()
deleteBlock hh = do
    deleteHeaderIndex hh
    DB{..} <- getBlockDataDB
    Rocks.delete rocksDB rocksWriteOpts (blockKeyMod . sformat hashHexF $ hh)
    Rocks.delete rocksDB rocksWriteOpts (undoKeyMod . sformat hashHexF $ hh)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: MonadDB m
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutSerBlunds
    $ one ( CB.getBlockHeader $ Left blk
          , Serialized . serialize' $ bimap (serialize' @Block) serialize' (Left blk, genesisUndo))
  where
    genesisUndo =
        Undo
        { undoTx = mempty
        , undoDlg = DlgUndo mempty mempty
        , undoUS = def
        , undoSlog = SlogUndo Nothing
        }

----------------------------------------------------------------------------
-- Pure implementation
----------------------------------------------------------------------------

dbGetSerBlockPureDefault
    :: (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockPureDefault h = do
    (serblund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeFull' @(ByteString, ByteString) <$> serblund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure . Just . Serialized $ fst v

dbGetSerUndoPureDefault
    :: forall ctx m. (MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoPureDefault h =  do
    (serblund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeFull' @(ByteString, ByteString) <$> serblund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure . Just . Serialized $ snd v

dbPutSerBlundsPureDefault ::
       forall ctx m. (MonadPureDB ctx m, MonadDB m)
    => NonEmpty (CB.BlockHeader, SerializedBlund)
    -> m ()
dbPutSerBlundsPureDefault (toList -> blunds) = do
    forM_ blunds $ \(bh, serBlund) -> do
        (var :: DBPureVar) <- view (lensOf @DBPureVar)
        flip atomicModifyIORefPure var $
            (pureBlocksStorage . at (headerHash bh) .~ Just (unSerialized serBlund))
    putHeadersIndex $ map fst blunds

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    )

dbGetSerBlockRealDefault ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe SerializedBlock)
dbGetSerBlockRealDefault x = Serialized <<$>> getSerializedBlock x

dbGetSerUndoRealDefault ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe SerializedUndo)
dbGetSerUndoRealDefault x = Serialized <<$>> getSerializedUndo x

dbPutSerBlundsRealDefault ::
       (MonadDB m, MonadRealDB ctx m)
    => NonEmpty (CB.BlockHeader, SerializedBlund)
    -> m ()
dbPutSerBlundsRealDefault = putSerializedBlunds

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDB m
    , MonadDBSum ctx m
    )

dbGetSerBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe SerializedBlock)
dbGetSerBlockSumDefault hh = eitherDB (dbGetSerBlockRealDefault hh) (dbGetSerBlockPureDefault hh)

dbGetSerUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe SerializedUndo)
dbGetSerUndoSumDefault hh =
    eitherDB (dbGetSerUndoRealDefault hh) (dbGetSerUndoPureDefault hh)

dbPutSerBlundsSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => NonEmpty (CB.BlockHeader, SerializedBlund) -> m ()
dbPutSerBlundsSumDefault b =
    eitherDB (dbPutSerBlundsRealDefault b) (dbPutSerBlundsPureDefault b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

undoKeyMod :: Text -> ByteString
undoKeyMod = ("undo/" <>) . encodeUtf8

blockKeyMod :: Text -> ByteString
blockKeyMod = ("block/" <>) . encodeUtf8
