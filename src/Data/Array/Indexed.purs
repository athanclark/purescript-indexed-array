module Data.Array.Indexed where

-- | Has a similar interface to Array, but has a unique String index
newtype IxArray a = IxArray
  { contents :: Object a
  , orderOfKeys :: Array String
  }

derive instance genericIxArray :: Genric a a' => Generic (IxArray a) _
derive newtype instance eqIxArray :: Eq a => Eq (IxArray a)
derive newtype instance showIxArray :: Show a => Show (IxArray a)
instance functorIxArray :: Functor IxArray where
  map f (IxArray x) = IxArray x
    { contents = map f contents
    }

empty :: forall a. IxArray a
empty = IxArray
  { contents: Object.empty
  , orderOfKeys: []
  }

insert :: forall a. String -> a -> IxArray a -> IxArray a
insert k a (IxArray {contents, orderOfKeys}) = IxArray
  { contents: Object.insert k a contents
  , orderOfKeys: if Array.elem k orderOfKeys then orderOfKeys else Array.snoc orderOfKeys k
  }

update :: forall a. 
