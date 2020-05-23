{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Free.Test where

import Context

import Control.Monad.Free
import Data.IntMap.Strict (IntMap)
import Test.QuickCheck.Gen (Gen (..), generate)
import Test.QuickCheck.GenT (MonadGen (..), frequency, oneof)

import qualified Data.Dependent.Map as DMap
import qualified Data.Dependent.Sum as DSum
import qualified Data.IntMap.Strict as IntMap

--------------------------------------------------------------------------------
-- Class and concrete

class Monad m => MonadStore m where
  put :: Int -> Char -> m ()
  get :: Int -> m Char

--------------------------------------------------------------------------------
-- Free

data StoreF a =
    Put Int Char a
  | Get Int (Char -> a)
  deriving (Functor)

type Store = Free StoreF

instance MonadStore (Free StoreF) where
  put n c = wrap $ Put n c $ pure ()
  get n = wrap $ Get n pure

eval :: MonadStore m => Store a -> m a
eval = foldFree alg
  where
    alg (Put n c x) = put n c >> pure x
    alg (Get n f) = f <$> get n

showStore :: Store () -> String
showStore = showStore' 0

showStore' :: Show a => Int -> Store a -> String
showStore' _ (Pure a) = "pure " ++ show a
showStore' k (Free x) =
  case x of
    Put n c f ->
      "put " <> show n <> " " <> show c <> "\n" <> showStore' k f
    Get n f ->
      "x" <> show k <> " <- get " <> show n <> "\n" <> showStore' (k + 1) (f 'a')

test ::
  (MonadStore m1, MonadStore m2, Monad m, Eq a) =>
  Store a ->
  (m1 a -> m a) ->
  (m2 a -> m a) ->
  m Bool
test code run1 run2 = do
  a1 <- run1 $ eval code
  a2 <- run2 $ eval code
  pure $ a1 == a2

--------------------------------------------------------------------------------
-- Stuff

-- The generation idea is to represent some AST like:
--
-- @@
-- given
--   c1 = gen @Char
--   i1 = gen @Int
-- do
--   put i1 c1
--   c2 <- get i1
--   pure c2
-- @@
--
-- And to compare the variable context. At the end of any execution, the
-- context should be the same.

data StoreV a =
    PutV (Var Int) (Var Char) a
  | GetV (Var Int) (Var Char, a)

{- XXX: This has to:

    - start with generated vars
    - add new vars of bindings
    - I think this has to be monadic

work :: Vars -> StoreV a -> StoreF a
work ctx (PutV vi vc x) =
  Put (getFromVars vi ctx) (getFromVars vc ctx) x
work ctx (GetV vi (vc, f)) =
  Get (getFromVars vi ctx) (const f)
-}

type Expr = Free StoreV

showStoreV :: Show a => Expr a -> String
showStoreV (Pure a) = "pure " ++ show a
showStoreV (Free x) =
  case x of
    PutV n c f ->
      "put " <> show n <> " " <> show c <> "\n" <> showStoreV f
    GetV n (v, f) ->
      show v <> " <- get " <> show n <> "\n" <> showStoreV f


genExpr :: Vars -> Gen (Expr (), Vars)
genExpr ctx = runVarsT ctx genExpr'

genExpr' :: VarsT Gen (Expr ())
genExpr' =
  frequency
    [ (1, pure $ Pure ())
    , (3, Free <$> genStoreF)
    ]
  where
    genStoreF =
      oneof
        [ PutV <$> genVar <*> genVar <*> genExpr'
        , GetV <$> genVar <*> genPair
        ]
    genPair = do
      var <- getFreeVar
      expr <- genExpr'
      pure (var, expr)

showRandomExpr :: IO ()
showRandomExpr = do
  let ctx = varsAdd @Int 2 $ varsAdd @Char 2 varsEmpty
  (expr, ctx) <- generate $ genExpr ctx
  putStrLn "Generated expression:\n"
  putStrLn $ showStoreV expr
  putStrLn ""
  putStrLn "Evaluation:\n"
