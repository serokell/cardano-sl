{-# LANGUAGE Rank2Types #-}

-- | Utilities related to 'lens' package.

module Pos.Util.Lens
       (
       -- * Custom lenses
         _neHead
       , _neTail
       , _neLast
       , listLens

       -- * Custom LensRules
       , postfixLFields
       , postfixLFields2

       ) where

import           Universum hiding (init, last)
import           Data.List (init, last)

import           Control.Applicative (ZipList (..))
import           Control.Lens (Iso', LensRules, coerced, from, lens, lensField, lensRules,
                               mappingNamer)

-- | Lens for the head of 'NonEmpty'.
--
-- We can't use '_head' because it doesn't work for 'NonEmpty':
-- <https://github.com/ekmett/lens/issues/636#issuecomment-213981096>.
-- Even if we could though, it wouldn't be a lens, only a traversal.
_neHead :: Lens' (NonEmpty a) a
_neHead f (x :| xs) = (:| xs) <$> f x

-- | Lens for the tail of 'NonEmpty'.
_neTail :: Lens' (NonEmpty a) [a]
_neTail f (x :| xs) = (x :|) <$> f xs

-- | Lens for the last element of 'NonEmpty'.
_neLast :: Lens' (NonEmpty a) a
_neLast f (x :| []) = (:| []) <$> f x
_neLast f (x :| xs) = (\y -> x :| init xs ++ [y]) <$> f (last xs)

-- Something weird, but maybe it's useful, I dunno.
applicativeLens :: forall f s a b. Applicative f => Lens' s (f a) -> Lens' a b -> Lens' s (f b)
applicativeLens lfa lb = lens (map (view lb) . view lfa) setter
  where
    setter :: s -> f b -> s
    setter s fb = s & lfa %~ (\fa -> set lb <$> fb <*> fa)

-- | If you have a lens from something to list of 'a' and from 'a' to
-- 'b', this function will create a function from that /something/ to
-- list of 'b'.
listLens :: forall s a b. Lens' s [a] -> Lens' a b -> Lens' s [b]
listLens lensToList lensToB = l . from _ZipList
  where
    _ZipList :: forall x. Iso' [x] (ZipList x)
    _ZipList = coerced
    l :: Lens' s (ZipList b)
    l = applicativeLens (lensToList . _ZipList) lensToB

----------------------------------------------------------------------------
-- Custom LensRules
----------------------------------------------------------------------------

postfixLFields :: LensRules
postfixLFields = lensRules & lensField .~ mappingNamer (\s -> [s++"_L"])

postfixLFields2 :: LensRules
postfixLFields2 = lensRules & lensField .~ mappingNamer (\s -> [s++"L"])
