{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Context
  ( -- * Variables
    Var
  , genVar
    -- * Sets of variables
  , Vars
  , varsEmpty
  , varsAdd
  , HasVars (..)
  , VarsT
  , runVarsT
    -- * Sets of variable bindings
  , Vals
  , valsFromVars
  , HasVals (..)
  , ValsT
  , runValsT
  ) where

import Control.Monad.State.Strict
  ( MonadState (..)
  , StateT
  , runStateT
  , mapStateT
  , modify'
  )
import Control.Monad.Trans (MonadTrans (..))
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Type.Reflection (Typeable, TypeRep, typeRep)
import Test.QuickCheck.GenT (MonadGen (..), elements)

import qualified Data.Dependent.Map as DMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

--------------------------------------------------------------------------------
-- Variables

newtype Var a = Var Int
  deriving (Eq)

instance Typeable a => Show (Var a) where
  show (Var n) =
    "var_" <> show (typeRep @a) <> "_" <> show n

--------------------------------------------------------------------------------
-- Sets of variables

newtype VarsFor a = VarsFor IntSet
  deriving (Show, Eq)

newtype Vars = Vars (DMap.DMap TypeRep VarsFor)

varsEmpty :: Vars
varsEmpty = Vars DMap.empty

varsAdd :: forall a. Typeable a => Int -> Vars -> Vars
varsAdd n (Vars vars) =
  let
    rep = typeRep @a
    varsFor =
      case DMap.lookup rep vars of
        Nothing ->
          VarsFor (IntSet.fromList [1..n])
        Just (VarsFor s) ->
          VarsFor (IntSet.fromList [1..n] <> s)
  in
    Vars $ DMap.insert rep varsFor vars

varsList :: forall a. Typeable a => Vars -> [Var a]
varsList (Vars vars) =
  let
    rep = typeRep @a
    VarsFor s = vars DMap.! rep
  in
    Var <$> IntSet.toList s

varsFree :: forall a. Typeable a => Vars -> (Var a, Vars)
varsFree (Vars vars) =
  let
    rep = typeRep @a
  in
    case DMap.lookup rep vars of
      Nothing ->
        let
          varsFor = VarsFor (IntSet.singleton 1)
        in
          (Var 1, Vars $ DMap.insert rep varsFor vars)
      Just (VarsFor s) ->
        let
          n = IntSet.findMax s + 1
          varsFor = VarsFor (IntSet.insert n s)
        in
          (Var n, Vars $ DMap.insert rep varsFor vars)

class HasVars m where
  getVars :: Typeable a => m [Var a]
  getFreeVar :: Typeable a => m (Var a)

newtype VarsT m a = VarsT (StateT Vars m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runVarsT :: Vars -> VarsT m a -> m (a, Vars)
runVarsT ctx (VarsT act) = runStateT act ctx

instance Monad m => HasVars (VarsT m) where
  getVars = VarsT $ varsList <$> get
  getFreeVar = VarsT $ state varsFree

instance MonadGen m => MonadGen (VarsT m) where
  liftGen gen =
    lift $ liftGen gen

  variant n (VarsT act) =
    VarsT $ mapStateT (variant n) act

  resize n (VarsT act) =
    VarsT $ mapStateT (resize n) act

  choose p =
    lift $ choose p

  sized f = do
    ctx <- VarsT $ get
    let f' = fmap fst . runVarsT ctx . f
    lift $ sized f'

genVar :: (HasVars m, MonadGen m, Typeable a) => m (Var a)
genVar = do
  vars <- getVars
  elements vars

--------------------------------------------------------------------------------
-- Sets of variable bindings

newtype ValsFor a = ValsFor (IntMap a)
  deriving (Show, Eq)

newtype Vals = Vals (DMap.DMap TypeRep ValsFor)

valsFromVars ::
  Applicative m =>
  Vars -> (forall a. Var a -> m a) -> m Vals
valsFromVars (Vars vars) f =
  let
    nat (VarsFor s) =
      fmap (ValsFor . IntMap.fromList) $
      traverse (\n -> (n,) <$> f (Var n)) $
      IntSet.toList s
  in
    Vals <$> DMap.traverseWithKey (\_ -> nat) vars

valsGet :: forall a. Typeable a => Var a -> Vals -> a
valsGet (Var n) (Vals vals) =
  let
    rep = typeRep @a

    ValsFor valsFor = vals DMap.! rep
  in
    valsFor IntMap.! n

valsPut :: forall a. Typeable a => Var a -> a -> Vals -> Vals
valsPut (Var n) a (Vals vals) =
  let
    rep = typeRep @a

    valsFor =
      case DMap.lookup rep vals of
        Nothing ->
          ValsFor (IntMap.singleton n a)
        Just (ValsFor m) ->
          ValsFor (IntMap.insert n a m)

  in
    Vals (DMap.insert rep valsFor vals)

class Monad m => HasVals m where
  getVal :: Typeable a => Var a -> m a
  putVal :: Typeable a => Var a -> a -> m ()

newtype ValsT m a = ValsT (StateT Vals m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runValsT :: Vals -> ValsT m a -> m (a, Vals)
runValsT ctx (ValsT act) = runStateT act ctx

instance Monad m => HasVals (ValsT m) where
  getVal var = ValsT $ valsGet var <$> get
  putVal var val = ValsT $ modify' (valsPut var val)

instance MonadGen m => MonadGen (ValsT m) where
  liftGen gen =
    lift $ liftGen gen

  variant n (ValsT act) =
    ValsT $ mapStateT (variant n) act

  resize n (ValsT act) =
    ValsT $ mapStateT (resize n) act

  choose p =
    lift $ choose p

  sized f = do
    ctx <- ValsT $ get
    let f' = fmap fst . runValsT ctx . f
    lift $ sized f'
