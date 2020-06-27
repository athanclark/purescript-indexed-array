module Test.Main where

import Data.Array.Indexed (IxArray, fromFoldable, toArray, length, insertAt, lookupWithIndex, keys, cons, snoc)

import Prelude
import Data.Array (elem) as Array
import Data.Maybe (Maybe (..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheck, quickCheckGen, Result (..), arbitrary)
import Test.QuickCheck.Gen (Gen, suchThat, chooseInt)
import Test.QuickCheck.UTF8String (genString)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.FunctorWithIndex (checkFunctorWithIndex)
import Test.QuickCheck.Laws.Data.Foldable (checkFoldableFunctor)
import Type.Proxy (Proxy (..), Proxy2 (..))

main :: Effect Unit
main = do
  log "fromFoldable <<< toFoldable"
  quickCheck fromToIso
  log "insertion exists"
  quickCheckGen insertExists
  log "cons exists and is 0"
  quickCheckGen consExists
  log "snoc exists and is length"
  quickCheckGen snocExists
  -- TODO json & arraybuffer serialization
  checkEq (Proxy :: Proxy (IxArray Int))
  checkSemigroup (Proxy :: Proxy (IxArray Int))
  checkMonoid (Proxy :: Proxy (IxArray Int))
  checkFunctor (Proxy2 :: Proxy2 IxArray)
  checkFunctorWithIndex (Proxy2 :: Proxy2 IxArray)
  checkFoldableFunctor (Proxy2 :: Proxy2 IxArray)

fromToIso :: IxArray Int -> Boolean
fromToIso xs =
  xs == fromFoldable (toArray xs)

insertExists :: Gen Result
insertExists = do
  xs <- arbitrary :: Gen (IxArray Int)
  x <- arbitrary
  s <- genString `suchThat` (\k -> not (Array.elem k (keys xs)))
  i <- chooseInt 0 (length xs)
  pure $ case insertAt i s x xs of
    Nothing -> Failed $ "Can't insert index! i: " <> show i <> ", xs: " <> show xs
    Just ys -> case lookupWithIndex s ys of
      Nothing -> Failed $ "No key in set! key: " <> s <> ", set: " <> show ys
      Just {index,value}
        | index == i && value == x -> Success
        | otherwise -> Failed $ "Index and value don't match! index: " <> show index <> ", i: " <> show i <> ", value: " <> show value <> ", x: " <> show x <> ", ys: " <> show ys

consExists :: Gen Result
consExists = do
  xs <- arbitrary :: Gen (IxArray Int)
  x <- arbitrary
  s <- genString `suchThat` (\k -> not (Array.elem k (keys xs)))
  pure $
    let ys = cons s x xs
    in  case lookupWithIndex s ys of
      Nothing -> Failed $ "No key in set! key: " <> s <> ", set: " <> show ys
      Just {index,value}
        | index == 0 && value == x -> Success
        | otherwise -> Failed $ "Index and value don't match! index: " <> show index <> ", value: " <> show value <> ", x: " <> show x <> ", ys: " <> show ys

snocExists :: Gen Result
snocExists = do
  xs <- arbitrary :: Gen (IxArray Int)
  x <- arbitrary
  s <- genString `suchThat` (\k -> not (Array.elem k (keys xs)))
  pure $
    let ys = snoc s xs x
    in  case lookupWithIndex s ys of
      Nothing -> Failed $ "No key in set! key: " <> s <> ", set: " <> show ys
      Just {index,value}
        | index == length xs && value == x -> Success
        | otherwise -> Failed $ "Index and value don't match! index: " <> show index <> ", value: " <> show value <> ", x: " <> show x <> ", ys: " <> show ys
