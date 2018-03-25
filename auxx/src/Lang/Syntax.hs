{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Lang.Syntax
       ( Expr(..)
       , Lit(..)
       , ProcCall(..)
       , Arg(..)
       , AtLeastTwo(..)
       , toList_
       , fromList_
       ) where

import           Universum

import           Data.Scientific (Scientific)

import           Lang.Name (Name)
import           Pos.Core (Address, BlockVersion, SoftwareVersion, StakeholderId)
import           Pos.Crypto (AHash, PublicKey)

data Expr cmd
    = ExprUnit
    | ExprGroup (AtLeastTwo (Expr cmd))
    | ExprProcCall (ProcCall cmd (Expr cmd))
    | ExprLit Lit

deriving instance Eq cmd => Eq (Expr cmd)
deriving instance Ord cmd => Ord (Expr cmd)
deriving instance Show cmd => Show (Expr cmd)
deriving instance Generic cmd => Generic (Expr cmd)

data AtLeastTwo a = AtLeastTwo a a [a]
  deriving (Functor, Foldable, Traversable, Generic, Eq, Ord, Show)

toList_ :: AtLeastTwo a -> [a]
toList_ (AtLeastTwo x y zs) = x:y:zs

-- UNSAFE
fromList_ :: [a] -> AtLeastTwo a
fromList_ (x:y:xs) = AtLeastTwo x y xs

data Lit
    = LitNumber Scientific
    | LitString String
    | LitAddress Address
    | LitPublicKey PublicKey
    | LitStakeholderId StakeholderId
    | LitHash AHash
    | LitBlockVersion BlockVersion
    | LitSoftwareVersion SoftwareVersion
    | LitFilePath FilePath
    deriving (Eq, Ord, Show, Generic)

data ProcCall cmd a = ProcCall cmd [Arg a]
    deriving (Functor, Foldable, Traversable)

deriving instance (Eq cmd, Eq a) => Eq (ProcCall cmd a)
deriving instance (Ord cmd, Ord a) => Ord (ProcCall cmd a)
deriving instance (Show cmd, Show a) => Show (ProcCall cmd a)
deriving instance (Generic cmd, Generic a) => Generic (ProcCall cmd a)


data Arg a = ArgPos a | ArgKw Name a
    deriving (Functor, Foldable, Traversable)

deriving instance Eq a => Eq (Arg a)
deriving instance Ord a => Ord (Arg a)
deriving instance Show a => Show (Arg a)
deriving instance Generic a => Generic (Arg a)
