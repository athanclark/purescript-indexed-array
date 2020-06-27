module Data.Array.Indexed where

import Prelude
import Data.Array
  ( foldr, foldl, foldMap, elem, snoc, cons, filter
  , toUnfoldable, index, elemIndex, insertAt, sortBy
  , length) as Array
import Data.Tuple (uncurry, Tuple (..))
import Data.Maybe (fromJust, isJust, Maybe (..))
import Data.Generic.Rep (class Generic)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Unfoldable (class Unfoldable)
import Foreign.Object (Object)
import Foreign.Object (lookup, insert, empty, union, filterKeys, toUnfoldable) as Object
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf)
import Test.QuickCheck.UTF8String (genString)

-- | Has a similar interface to Array, but has a unique String index
newtype IxArray a = IxArray
  { contents :: Object a
  , orderOfKeys :: Array String
  }

derive instance genericIxArray :: Generic a a' => Generic (IxArray a) _
derive newtype instance eqIxArray :: Eq a => Eq (IxArray a)
derive newtype instance showIxArray :: Show a => Show (IxArray a)
instance semigroupIxArray :: Semigroup (IxArray a) where
  append (IxArray x) (IxArray y) = IxArray
    { contents: Object.union y.contents x.contents
    , orderOfKeys: Array.filter (\z -> not (Array.elem z y.orderOfKeys)) x.orderOfKeys <> y.orderOfKeys
    }
instance monoidIxArray :: Monoid (IxArray a) where
  mempty = empty
instance functorIxArray :: Functor IxArray where
  map f (IxArray x) = IxArray x
    { contents = map f x.contents
    }
instance functorWithIndexIxArray :: FunctorWithIndex String IxArray where
  mapWithIndex f (IxArray x) = IxArray x
    { contents = mapWithIndex f x.contents
    }
instance foldableIxArray :: Foldable IxArray where
  foldr f acc (IxArray {contents, orderOfKeys}) =
    let getVal k = unsafePartial (fromJust (Object.lookup k contents))
    in  Array.foldr f acc (map getVal orderOfKeys)
  foldl f acc (IxArray {contents, orderOfKeys}) =
    let getVal k = unsafePartial (fromJust (Object.lookup k contents))
    in  Array.foldl f acc (map getVal orderOfKeys)
  foldMap f (IxArray {contents, orderOfKeys}) =
    let getVal k = unsafePartial (fromJust (Object.lookup k contents))
    in  Array.foldMap f (map getVal orderOfKeys)
instance foldableWithIndexIxArray :: FoldableWithIndex String IxArray where
  foldrWithIndex f acc (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
    in  Array.foldr (uncurry f) acc (map getVal orderOfKeys)
  foldlWithIndex f acc (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
    in  Array.foldl (\acc' (Tuple i x) -> f i acc' x) acc (map getVal orderOfKeys)
  foldMapWithIndex f (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
    in  Array.foldMap (uncurry f) (map getVal orderOfKeys)
instance traversableIxArray :: Traversable IxArray where
  sequence :: forall m a. Applicative m => IxArray (m a) -> m (IxArray a)
  sequence (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
        -- go :: m (IxArray a) -> Tuple String (m a) -> m (IxArray a)
        go xs (Tuple k x) = snocOrShift k <$> xs <*> x
    in  Array.foldl go (pure empty) (map getVal orderOfKeys)
  traverse :: forall m a b. Applicative m => (a -> m b) -> IxArray a -> m (IxArray b)
  traverse f (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
        -- go :: m (IxArray b) -> Tuple String a -> m (IxArray b)
        go xs (Tuple k x) = snocOrShift k <$> xs <*> f x
    in  Array.foldl go (pure empty) (map getVal orderOfKeys)
instance traverseableWithIndexIxArray :: TraversableWithIndex String IxArray where
  traverseWithIndex :: forall m a b. Applicative m => (String -> a -> m b) -> IxArray a -> m (IxArray b)
  traverseWithIndex f (IxArray {contents, orderOfKeys}) =
    let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
        -- go :: m (IxArray a) -> Tuple String a -> m (IxArray b)
        go xs (Tuple k x) = snocOrShift k <$> xs <*> f k x
    in  Array.foldl go (pure empty) (map getVal orderOfKeys)
instance arbitraryIxArray :: Arbitrary a => Arbitrary (IxArray a) where
  arbitrary = fromFoldable <$> arrayOf (Tuple <$> genString <*> arbitrary)

empty :: forall a. IxArray a
empty = IxArray
  { contents: Object.empty
  , orderOfKeys: []
  }

length :: forall a. IxArray a -> Int
length (IxArray {orderOfKeys}) = Array.length orderOfKeys

findKey :: forall a. Eq a => a -> IxArray a -> Maybe String
findKey x (IxArray {contents}) =
  let xs :: Array _
      xs = Object.toUnfoldable contents
      go :: Tuple String a -> Maybe String -> Maybe String
      go (Tuple k y) acc
        | y == x = Just k
        | otherwise = acc
  in  Array.foldr go Nothing xs

elem :: forall a. Eq a => a -> IxArray a -> Boolean
elem x = isJust <<< findKey x

elemKey :: forall a. String -> IxArray a -> Boolean
elemKey k (IxArray {orderOfKeys}) = Array.elem k orderOfKeys

filter :: forall a. (a -> Boolean) -> IxArray a -> IxArray a
filter f = filterWithKey (\_ x -> f x)

filterWithKey :: forall a. (String -> a -> Boolean) -> IxArray a -> IxArray a
filterWithKey f (IxArray {contents, orderOfKeys}) = IxArray
  let toDelete :: Array String
      toDelete =
        let go :: Tuple String a -> Array String -> Array String
            go (Tuple k x) acc
              | f k x = Array.snoc acc k
              | otherwise = acc
        in  Array.foldr go [] (Object.toUnfoldable contents :: Array _)
  in  { contents: Object.filterKeys (\k -> Array.elem k toDelete) contents
      , orderOfKeys: Array.filter (\k -> Array.elem k toDelete) orderOfKeys
      }

toUnfoldable :: forall a f. Unfoldable f => IxArray a -> f (Tuple String a)
toUnfoldable = Array.toUnfoldable <<< toArray

toArray :: forall a. IxArray a -> Array (Tuple String a)
toArray (IxArray {contents, orderOfKeys}) =
  let getVal k = Tuple k (unsafePartial (fromJust (Object.lookup k contents)))
  in  map getVal orderOfKeys

fromFoldable :: forall a f. Foldable f => f (Tuple String a) -> IxArray a
fromFoldable = foldr (uncurry consOrShift) empty

-- | Appends the element if it doesn't already exist.
snoc :: forall a. String -> IxArray a -> a -> IxArray a
snoc k (IxArray {contents, orderOfKeys}) a = IxArray
  { contents: Object.insert k a contents
  , orderOfKeys: if Array.elem k orderOfKeys then orderOfKeys else Array.snoc orderOfKeys k
  }

-- | Always appends the element to the end of the array.
snocOrShift :: forall a. String -> IxArray a -> a -> IxArray a
snocOrShift k (IxArray {contents, orderOfKeys}) a = IxArray
  { contents: Object.insert k a contents
  , orderOfKeys: Array.snoc (Array.filter (_ /= k) orderOfKeys) k
  }

snocWith :: forall a. String -> (a -> a) -> IxArray a -> a -> IxArray a
snocWith k f (IxArray {contents, orderOfKeys}) a = IxArray
  { contents: case Object.lookup k contents of
      Nothing -> Object.insert k a contents
      Just b -> Object.insert k (f b) contents
  , orderOfKeys: if Array.elem k orderOfKeys then orderOfKeys else Array.snoc orderOfKeys k
  }

snocOrShiftWith :: forall a. String -> (a -> a) -> IxArray a -> a -> IxArray a
snocOrShiftWith k f (IxArray {contents, orderOfKeys}) a = IxArray
  { contents: case Object.lookup k contents of
      Nothing -> Object.insert k a contents
      Just b -> Object.insert k (f b) contents
  , orderOfKeys: Array.snoc (Array.filter (_ /= k) orderOfKeys) k
  }

-- | Prepends the element if it doesn't already exist.
cons :: forall a. String -> a -> IxArray a -> IxArray a
cons k a (IxArray {contents, orderOfKeys}) = IxArray
  { contents: Object.insert k a contents
  , orderOfKeys: if Array.elem k orderOfKeys then orderOfKeys else Array.cons k orderOfKeys
  }

-- | Always prepends the element to the end of the array.
consOrShift :: forall a. String -> a -> IxArray a -> IxArray a
consOrShift k a (IxArray {contents, orderOfKeys}) = IxArray
  { contents: Object.insert k a contents
  , orderOfKeys: Array.cons k (Array.filter (_ /= k) orderOfKeys)
  }

consWith :: forall a. String -> (a -> a) -> a -> IxArray a -> IxArray a
consWith k f a (IxArray {contents, orderOfKeys}) = IxArray
  { contents: case Object.lookup k contents of
      Nothing -> Object.insert k a contents
      Just b -> Object.insert k (f b) contents
  , orderOfKeys: if Array.elem k orderOfKeys then orderOfKeys else Array.cons k orderOfKeys
  }

consOrShiftWith :: forall a. String -> (a -> a) -> a -> IxArray a -> IxArray a
consOrShiftWith k f a (IxArray {contents, orderOfKeys}) = IxArray
  { contents: case Object.lookup k contents of
      Nothing -> Object.insert k a contents
      Just b -> Object.insert k (f b) contents
  , orderOfKeys: Array.cons k (Array.filter (_ /= k) orderOfKeys)
  }

-- | Fails when key already exists
insertAt :: forall a. Int -> String -> a -> IxArray a -> Maybe (IxArray a)
insertAt i k x (IxArray {contents, orderOfKeys})
  | not (Array.elem k orderOfKeys) = case Array.insertAt i k orderOfKeys of
      Nothing -> Nothing
      Just orderOfKeys' -> Just (IxArray {orderOfKeys: orderOfKeys', contents: Object.insert k x contents})
  | otherwise = Nothing

-- | Overwrites when key already exists. May offset expected index!
insertAt' :: forall a. Int -> String -> a -> IxArray a -> Maybe (IxArray a)
insertAt' i k x (IxArray {contents, orderOfKeys}) = case Array.insertAt i k (Array.filter (_ /= k) orderOfKeys) of
  Nothing -> Nothing
  Just orderOfKeys' -> Just (IxArray {orderOfKeys: orderOfKeys', contents: Object.insert k x contents})

keys :: forall a. IxArray a -> Array String
keys (IxArray {orderOfKeys}) = orderOfKeys

lookup :: forall a. String -> IxArray a -> Maybe a
lookup k (IxArray {contents}) = Object.lookup k contents

lookupWithIndex :: forall a. String -> IxArray a -> Maybe {index :: Int, value :: a}
lookupWithIndex k xs@(IxArray {orderOfKeys}) = case lookup k xs of
  Nothing -> Nothing
  Just value -> case Array.elemIndex k orderOfKeys of
    Nothing -> Nothing
    Just index' -> Just {index: index',value}

index :: forall a. Int -> IxArray a -> Maybe {key :: String, value :: a}
index i xs@(IxArray {orderOfKeys}) = case Array.index orderOfKeys i of
  Nothing -> Nothing
  Just key -> case lookup key xs of
    Nothing -> Nothing
    Just value -> Just {key,value}

sortKeysBy :: forall a. (String -> String -> Ordering) -> IxArray a -> IxArray a
sortKeysBy f (IxArray x) = IxArray x {orderOfKeys = Array.sortBy f x.orderOfKeys}

sortBy :: forall a. (a -> a -> Ordering) -> IxArray a -> IxArray a
sortBy f = sortByWithKey (\(Tuple _ x) (Tuple _ y) -> f x y)

sortByWithKey :: forall a. (Tuple String a -> Tuple String a -> Ordering) -> IxArray a -> IxArray a
sortByWithKey f xs@(IxArray {contents}) = sortKeysBy (\x y -> f (Tuple x (getVal x)) (Tuple y (getVal y))) xs
  where
    getVal k = unsafePartial (fromJust (Object.lookup k contents))
