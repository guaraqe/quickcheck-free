{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Free.Test where

import Control.Monad.Free
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen (..))

--------------------------------------------------------------------------------
-- Class and concrete

class Monad m => MonadStore m where
  put :: Int -> Char -> m ()
  get :: Int -> m Char

--------------------------------------------------------------------------------
-- Var

data Var
  = VarInt Int
  | VarChar Char
  deriving (Show)

genVar :: Gen Var
genVar =
  oneof
    [ VarInt <$> choose (0,5)
    , VarChar <$> choose ('a','f')
    ]

genInt :: Gen Var -> Gen Int
genInt gen = suchThatMap gen var
  where
    var (VarInt n) = Just n
    var _ = Nothing

genChar :: Gen Var -> Gen Char
genChar gen = suchThatMap gen var
  where
    var (VarChar n) = Just n
    var _ = Nothing


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

showStore :: Store Var -> String
showStore = showStore' 0

showStore' :: Show a => Int -> Store a -> String
showStore' _ (Pure a) = "pure " ++ show a
showStore' k (Free x) =
  case x of
    Put n c f ->
      "put " <> show n <> " " <> show c <> "\n" <> showStore' k f
    Get n f ->
      "x" <> show k <> " <- get " <> show n <> "\n" <> showStore' (k + 1) (f 'a')

--------------------------------------------------------------------------------
-- Free generation

genFree :: Gen (Store Var)
genFree = genFree' genVar

genFree' :: Gen Var -> Gen (Store Var)
genFree' gen =
  frequency
    [ (1, Pure <$> gen)
    , (3, Free <$> genStoreF)
    ]
  where
    genStoreF :: Gen (StoreF (Store Var))
    genStoreF =
      oneof
        [ Put <$> genInt gen <*> genChar gen <*> genFree' gen
        , Get <$> genInt gen <*> genFun
        ]

    genFun :: Gen (Char -> Store Var)
    genFun = shuffleGen $ \c ->
      genFree' $ oneof [ gen, pure (VarChar c) ]

shuffleGen :: (a -> Gen b) -> Gen (a -> b)
shuffleGen f = MkGen $ \g n -> \x ->
  let
    MkGen h = f x
  in
    h g n

--------------------------------------------------------------------------------
-- Test

code :: MonadStore m => m Bool
code = do
  put 0 'a'
  put 0 'b'
  n1 <- get 0
  n2 <- get 1
  pure $ n1 == n2

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
    PutV (VarV Int) (VarV Char) a
  | GetV (VarV Int) (VarV Char -> a)

newtype VarV a = Var String
