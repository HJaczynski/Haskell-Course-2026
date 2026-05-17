import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

-- Task 1: Stack machine
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
    stack <- get
    case stack of
        [] -> return ()
        (_:xs) -> put xs
execInstr DUP = do
    stack <- get
    case stack of
        [] -> return ()
        (x:xs) -> put (x : x : xs)
execInstr SWAP = do
    stack <- get
    case stack of
        (x:y:xs) -> put (y : x : xs)
        _ -> return ()
execInstr ADD = do
    stack <- get
    case stack of
        (x:y:xs) -> put (x + y : xs)
        _ -> return ()
execInstr MUL = do
    stack <- get
    case stack of
        (x:y:xs) -> put (x * y : xs)
        _ -> return ()
execInstr NEG = do
    stack <- get
    case stack of
        [] -> return ()
        (x:xs) -> put (-x : xs)

execProg :: [Instr] -> State [Int] ()
execProg [] = return ()
execProg (instr:instrs) = do
    execInstr instr
    execProg instrs

runProg :: [Instr] -> [Int]
runProg instrs = execState (execProg instrs) []

-- Task 2: Expression evaluator with variable bindings
data Expr
    = Num Int
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Neg Expr
    | Assign String Expr
    | Seq Expr Expr
  deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = return n
eval (Var name) = do
    env <- get
    return (env Map.! name)
eval (Add expr1 expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    return (value1 + value2)
eval (Mul expr1 expr2) = do
    value1 <- eval expr1
    value2 <- eval expr2
    return (value1 * value2)
eval (Neg expr) = do
    value <- eval expr
    return (-value)
eval (Assign name expr) = do
    value <- eval expr
    modify (Map.insert name value)
    return value
eval (Seq expr1 expr2) = do
    _ <- eval expr1
    eval expr2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

-- Task 3: Memoised edit distance
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
    cache <- get
    case Map.lookup (i, j) cache of
        Just value -> return value
        Nothing -> do
            value <- computeDistance
            modify (Map.insert (i, j) value)
            return value
  where
    computeDistance
        | i == 0 = return j
        | j == 0 = return i
        | xs !! (i - 1) == ys !! (j - 1) = editDistM xs ys (i - 1) (j - 1)
        | otherwise = do
            deleteCost <- editDistM xs ys (i - 1) j
            insertCost <- editDistM xs ys i (j - 1)
            replaceCost <- editDistM xs ys (i - 1) (j - 1)
            return (1 + minimum [deleteCost, insertCost, replaceCost])

editDistance :: String -> String -> Int
editDistance xs ys =
    evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- Task 4, 5, 6: Treasure Hunters game
data Place
    = Start
    | Normal
    | DecisionPoint [String]
    | Obstacle Int
    | Treasure Int
    | Trap Int
    | Goal
  deriving (Show)

data GameState = GameState
    { currentRouteName :: String
    , currentRoute :: [Int]
    , routeIndex :: Int
    , playerEnergy :: Int
    , playerScore :: Int
    , handledPlaces :: [Int]
    } deriving (Show)

type AdventureGame a = StateT GameState IO a

initialGameState :: GameState
initialGameState =
    GameState
        { currentRouteName = "start"
        , currentRoute = [0, 1, 2, 3]
        , routeIndex = 0
        , playerEnergy = 12
        , playerScore = 0
        , handledPlaces = []
        }

currentPosition :: GameState -> Int
currentPosition state = currentRoute state !! routeIndex state

placeAt :: Int -> Place
placeAt 0 = Start
placeAt 1 = Treasure 5
placeAt 2 = Obstacle 2
placeAt 3 = DecisionPoint ["forest", "cave", "river"]
placeAt 4 = Treasure 10
placeAt 5 = Obstacle 3
placeAt 6 = Goal
placeAt 7 = Trap 4
placeAt 8 = Treasure 7
placeAt 9 = Obstacle 1
placeAt 10 = Trap 2
placeAt _ = Normal

routeForChoice :: String -> [Int]
routeForChoice "forest" = [3, 4, 5, 6]
routeForChoice "cave" = [3, 7, 8, 6]
routeForChoice "river" = [3, 9, 10, 6]
routeForChoice _ = [3, 4, 5, 6]

movePlayer :: Int -> AdventureGame Int
movePlayer diceRoll = do
    state <- get
    let spacesLeft = length (currentRoute state) - 1 - routeIndex state
    let stepsByEnergy = min diceRoll (playerEnergy state)
    let actualSteps = min stepsByEnergy spacesLeft
    put state
        { routeIndex = routeIndex state + actualSteps
        , playerEnergy = playerEnergy state - actualSteps
        }
    return actualSteps

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
    choice <- liftIO (getPlayerChoice options)
    return choice

handleLocation :: AdventureGame Bool
handleLocation = do
    state <- get
    let pos = currentPosition state
    let place = placeAt pos
    case place of
        Goal -> do
            liftIO (putStrLn "You found the main treasure.")
            return True
        DecisionPoint options ->
            if currentRouteName state == "start"
                then do
                    choice <- makeDecision options
                    put state
                        { currentRouteName = choice
                        , currentRoute = routeForChoice choice
                        , routeIndex = 0
                        }
                    liftIO (putStrLn ("You chose the " ++ choice ++ " path."))
                    return False
                else return False
        Treasure points ->
            if pos `elem` handledPlaces state
                then return False
                else do
                    put state
                        { playerScore = playerScore state + points
                        , handledPlaces = pos : handledPlaces state
                        }
                    liftIO (putStrLn ("You found a treasure worth " ++ show points ++ " points."))
                    return False
        Trap points ->
            if pos `elem` handledPlaces state
                then return False
                else do
                    let newScore = max 0 (playerScore state - points)
                    put state
                        { playerScore = newScore
                        , handledPlaces = pos : handledPlaces state
                        }
                    liftIO (putStrLn ("A trap took away " ++ show points ++ " points."))
                    return False
        Obstacle penalty ->
            if pos `elem` handledPlaces state
                then return False
                else do
                    let newEnergy = max 0 (playerEnergy state - penalty)
                    put state
                        { playerEnergy = newEnergy
                        , handledPlaces = pos : handledPlaces state
                        }
                    liftIO (putStrLn ("An obstacle cost you " ++ show penalty ++ " extra energy."))
                    return False
        _ -> return False

playTurn :: AdventureGame Bool
playTurn = do
    state <- get
    if playerEnergy state <= 0
        then do
            liftIO (putStrLn "You have no energy left.")
            return True
        else if currentPosition state == 6
            then do
                liftIO (putStrLn "You are already at the goal.")
                return True
            else do
                diceRoll <- liftIO getDiceRoll
                moved <- movePlayer diceRoll
                liftIO (putStrLn ("You moved " ++ show moved ++ " spaces."))
                finished <- handleLocation
                newState <- get
                liftIO (displayGameState newState)
                if finished
                    then do
                        liftIO (putStrLn ("Final score: " ++ show (playerScore newState)))
                        return True
                    else if playerEnergy newState <= 0
                        then do
                            liftIO (putStrLn "You ran out of energy.")
                            return True
                        else return False

playGame :: AdventureGame ()
playGame = do
    state <- get
    liftIO (displayGameState state)
    go
  where
    go = do
        finished <- playTurn
        if finished
            then return ()
            else go

getDiceRoll :: IO Int
getDiceRoll = do
    putStrLn "Enter a dice roll from 1 to 6:"
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= 6 -> return n
        _ -> do
            putStrLn "Please enter a whole number between 1 and 6."
            getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState state = do
    putStrLn "------------------------------"
    putStrLn ("Route: " ++ currentRouteName state)
    putStrLn ("Position: " ++ show (currentPosition state))
    putStrLn ("Energy: " ++ show (playerEnergy state))
    putStrLn ("Score: " ++ show (playerScore state))
    putStrLn ("Place: " ++ describePlace (placeAt (currentPosition state)))
    putStrLn "------------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
    putStrLn "Choose a path:"
    showOptions 1 options
    input <- getLine
    case reads input of
        [(n, "")] | n >= 1 && n <= length options -> return (options !! (n - 1))
        _ -> do
            putStrLn "Please enter one of the option numbers."
            getPlayerChoice options
  where
    showOptions _ [] = return ()
    showOptions n (x:xs) = do
        putStrLn (show n ++ ". " ++ x)
        showOptions (n + 1) xs

describePlace :: Place -> String
describePlace Start = "start"
describePlace Normal = "normal place"
describePlace (DecisionPoint _) = "decision point"
describePlace (Obstacle _) = "obstacle"
describePlace (Treasure _) = "treasure"
describePlace (Trap _) = "trap"
describePlace Goal = "main treasure"

startGame :: IO ()
startGame = evalStateT playGame initialGameState

main :: IO ()
main = do
    print (runProg [PUSH 3, PUSH 4, ADD, DUP, NEG])
    print (runProg [POP, PUSH 2, SWAP, PUSH 5, MUL])

    let expr1 = Seq (Assign "x" (Num 10)) (Add (Var "x") (Num 5))
    let expr2 = Seq (Assign "x" (Num 3)) (Seq (Assign "y" (Mul (Var "x") (Num 4))) (Add (Var "x") (Var "y")))
    print (runEval expr1)
    print (runEval expr2)

    print (editDistance "kitten" "sitting")
    print (editDistance "flaw" "lawn")

    putStrLn "Initial game state:"
    displayGameState initialGameState