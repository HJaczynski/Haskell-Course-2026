import Control.Monad (guard)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map

-- Task 1: Maybe Monad
type Pos = (Int, Int)

data Dir = N | S | E | W
  deriving (Eq, Ord, Show)

type Maze = Map Pos (Map Dir Pos)

move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
    neighbours <- Map.lookup pos maze
    Map.lookup dir neighbours

followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath _ start [] = Just start
followPath maze start (dir:dirs) = do
    next <- move maze start dir
    followPath maze next dirs

safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath _ start [] = Just [start]
safePath maze start (dir:dirs) = do
    next <- move maze start dir
    rest <- safePath maze next dirs
    return (start : rest)

-- Task 2: Decoding a message
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key text = traverse (\c -> Map.lookup c key) text

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key wordsList = traverse (decrypt key) wordsList

-- Task 3: Seating arrangements
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
    arrangement <- permutations guests
    guard (isValid arrangement)
    return arrangement
  where
    isConflict a b = (a, b) `elem` conflicts || (b, a) `elem` conflicts

    isValid [] = True
    isValid [_] = True
    isValid xs = all validPair (zip xs (tail xs ++ [head xs]))

    validPair (a, b) = not (isConflict a b)

-- Task 4: Result monad with warnings
data Result a = Failure String | Success a [String]
  deriving (Show, Eq)

instance Functor Result where
    fmap _ (Failure msg) = Failure msg
    fmap f (Success x warnings) = Success (f x) warnings

instance Applicative Result where
    pure x = Success x []

    Failure msg <*> _ = Failure msg
    Success _ warnings <*> Failure msg = Failure msg
    Success f warnings1 <*> Success x warnings2 = Success (f x) (warnings1 ++ warnings2)

instance Monad Result where
    Failure msg >>= _ = Failure msg
    Success x warnings1 >>= f =
        case f x of
            Failure msg -> Failure msg
            Success y warnings2 -> Success y (warnings1 ++ warnings2)

warn :: String -> Result ()
warn message = Success () [message]

failure :: String -> Result a
failure message = Failure message

validateAge :: Int -> Result Int
validateAge age
    | age < 0 = failure "Age cannot be negative"
    | age > 150 = do
        warn "Age is above 150"
        return age
    | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges ages = mapM validateAge ages

-- Task 5: Writer Monad
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Neg expr) = do
    simpleExpr <- simplify expr
    case simpleExpr of
        Neg inner -> do
            tell ["Double negation: -(-e) -> e"]
            return inner
        _ -> return (Neg simpleExpr)
simplify (Add left right) = do
    simpleLeft <- simplify left
    simpleRight <- simplify right
    case (simpleLeft, simpleRight) of
        (Lit 0, expr) -> do
            tell ["Add identity: 0 + e -> e"]
            return expr
        (expr, Lit 0) -> do
            tell ["Add identity: e + 0 -> e"]
            return expr
        (Lit a, Lit b) -> do
            tell ["Add constant folding"]
            return (Lit (a + b))
        _ -> return (Add simpleLeft simpleRight)
simplify (Mul left right) = do
    simpleLeft <- simplify left
    simpleRight <- simplify right
    case (simpleLeft, simpleRight) of
        (Lit 0, _) -> do
            tell ["Mul zero: 0 * e -> 0"]
            return (Lit 0)
        (_, Lit 0) -> do
            tell ["Mul zero: e * 0 -> 0"]
            return (Lit 0)
        (Lit 1, expr) -> do
            tell ["Mul identity: 1 * e -> e"]
            return expr
        (expr, Lit 1) -> do
            tell ["Mul identity: e * 1 -> e"]
            return expr
        (Lit a, Lit b) -> do
            tell ["Mul constant folding"]
            return (Lit (a * b))
        _ -> return (Mul simpleLeft simpleRight)

-- Task 6: ZipList
newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Show, Eq)

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-- 6c) 
-- ZipList cannot have a Monad instance whose Applicative instance is the
-- positional one above. A Monad determines Applicative via ap, so (<*>)
-- would have to behave like monadic sequencing. But (>>=) has no lawful way
-- to preserve the original element positions when the function returns lists
-- of different lengths, so it cannot produce the zipping behaviour required
-- by this Applicative instance.

main :: IO ()
main = do
    let maze =
            Map.fromList
                [ ((0, 0), Map.fromList [(E, (1, 0)), (S, (0, 1))])
                , ((1, 0), Map.fromList [(W, (0, 0)), (S, (1, 1))])
                , ((0, 1), Map.fromList [(N, (0, 0)), (E, (1, 1))])
                , ((1, 1), Map.fromList [(N, (1, 0)), (W, (0, 1))])
                ]
    print (move maze (0, 0) E)
    print (move maze (0, 0) W)
    print (followPath maze (0, 0) [E, S])
    print (followPath maze (0, 0) [E, E])
    print (safePath maze (0, 0) [S, E])

    let key = Map.fromList [('a', 'x'), ('b', 'y'), ('c', 'z')]
    print (decrypt key "abc")
    print (decrypt key "abd")
    print (decryptWords key ["ab", "ca"])

    print (seatings ["Alice", "Bob", "Carol"] [("Alice", "Bob")])

    print (validateAge 25)
    print (validateAge 200)
    print (validateAges [20, 200, 30])
    print (validateAges [20, -5, 30])

    print (runWriter (simplify (Add (Lit 0) (Mul (Lit 1) (Lit 5)))))
    print (runWriter (simplify (Neg (Neg (Lit 3)))))
    print (runWriter (simplify (Mul (Add (Lit 2) (Lit 3)) (Lit 1))))

    print (pure id <*> ZipList [1, 2, 3] :: ZipList Int)
    print (pure (+) <*> ZipList [1, 2, 3] <*> ZipList [10, 20, 30] :: ZipList Int)
