import Data.Foldable (length, toList)
import Data.Monoid (Sum(..))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show, Eq)

-- Task 1: Functor for Sequence
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append left right) = Append (fmap f left) (fmap f right)

-- Task 2: Foldable for Sequence
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append left right) = foldMap f left <> foldMap f right

seqToList :: Sequence a -> [a]
seqToList xs = toList xs

seqLength :: Sequence a -> Int
seqLength xs = length xs

-- Task 3: Semigroup and Monoid for Sequence
instance Semigroup (Sequence a) where
    Empty <> ys = ys
    xs <> Empty = xs
    xs <> ys = Append xs ys

instance Monoid (Sequence a) where
    mempty = Empty

-- Task 4: Tail recursion and Sequence search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem value seq0 = go [seq0]
  where
    go [] = False
    go (current:stack) =
        case current of
            Empty -> go stack
            Single x -> x == value || go stack
            Append left right -> go (left : right : stack)


main :: IO ()
main = do
    let seq1 = Append (Single 1) (Append (Single 2) (Single 3))
    print (fmap (*2) seq1)
    print (foldMap Sum seq1)
    print (seqToList seq1)
    print (seqLength seq1)
    print (tailElem 2 seq1)
    print (tailElem 4 seq1)
