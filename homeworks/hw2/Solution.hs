{- HLINT ignore "Eta reduce" -}
import Data.Foldable (length, toList)
import Data.Monoid (Sum(..))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show)

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
            Single x -> (x == value) || go stack
            Append left right -> go (left : right : stack)

-- Task 5: Tail recursion and Sequence flatten
tailToList :: Sequence a -> [a]
tailToList seq0 = reverse (go [seq0] [])
  where
    go [] acc = acc
    go (current:stack) acc =
        case current of
            Empty -> go stack acc
            Single x -> go stack (x : acc)
            Append left right -> go (left : right : stack) acc

-- Task 6: Tail recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _ = Nothing
    go (token:rest) stack =
        case token of
            TNum n -> go rest (n : stack)
            TAdd -> applyOp (+) rest stack
            TSub -> applyOp (-) rest stack
            TMul -> applyOp (*) rest stack
            TDiv -> applyDiv rest stack

    applyOp op rest (x:y:stack) = go rest (y `op` x : stack)
    applyOp _ _ _ = Nothing

    applyDiv rest (x:y:stack)
        | x == 0 = Nothing
        | otherwise = go rest (y `div` x : stack)
    applyDiv _ _ = Nothing

-- Task 7: Expressing functions via folds
myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr step []
  where
    step x acc
        | p x = x : acc
        | otherwise = []

decimal :: [Int] -> Int
decimal = foldl (\acc digit -> acc * 10 + digit) 0

-- Task 8: Run-length encoding via folds
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ((y, n):ys)
        | x == y = (y, n + 1) : ys
        | otherwise = (x, 1) : (y, n) : ys

decode :: [(a, Int)] -> [a]
decode = foldr (\(x, n) acc -> replicate n x ++ acc) []

main :: IO ()
main = do
    let seq1 = Append (Single 1) (Append (Single 2) (Single 3))
    print (fmap (*2) seq1)
    print (foldMap Sum seq1)
    print (seqToList seq1)
    print (seqLength seq1)
    let s1 = Append (Single 1) (Single 2)
    let s2 = Append (Single 3) (Single 4)
    print (seqToList (s1 <> s2))
    print (seqToList (mempty <> s1))
    print (seqToList (s1 <> mempty))
    print (seqToList ((s1 <> s2) <> Single 5))
    print (seqToList (s1 <> (s2 <> Single 5)))
    print (tailElem 2 seq1)
    print (tailElem 4 seq1)
    print (tailToList seq1)
    print (tailRPN [TNum 2, TNum 3, TAdd])
    print (tailRPN [TNum 10, TNum 2, TDiv])
    print (tailRPN [TNum 4, TAdd])
    print (myReverse [1, 2, 3, 4])
    print (myTakeWhile even [2, 4, 6, 3, 8])
    print (decimal [1, 2, 3, 4])
    print (encode "aaabccca")
    print (decode [('a', 3), ('b', 1), ('c', 3), ('a', 1)])
