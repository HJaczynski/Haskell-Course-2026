newtype Reader r a = Reader { runReader :: r -> a }

-- Task 1: Functor, Applicative, and Monad instances
instance Functor (Reader r) where
    fmap f reader = Reader (\env -> f (runReader reader env))

instance Applicative (Reader r) where
    pure x = Reader (\_ -> x)

    liftA2 f reader1 reader2 =
        Reader (\env -> f (runReader reader1 env) (runReader reader2 env))

instance Monad (Reader r) where
    reader >>= f =
        Reader (\env -> runReader (f (runReader reader env)) env)

-- Task 2: Primitive operations
ask :: Reader r r
ask = Reader (\env -> env)

asks :: (r -> a) -> Reader r a
asks f = Reader (\env -> f env)

local :: (r -> r) -> Reader r a -> Reader r a
local change reader = Reader (\env -> runReader reader (change env))

-- Task 3: Banking system
data BankConfig = BankConfig
    { interestRate :: Double
    , transactionFee :: Int
    , minimumBalance :: Int
    } deriving (Show)

data Account = Account
    { accountId :: String
    , balance :: Int
    } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    return (round (fromIntegral (balance account) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    minBal <- asks minimumBalance
    return (balance account >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    updatedAccount <- applyTransactionFee account
    interest <- calculateInterest account
    hasEnoughMoney <- checkMinimumBalance account
    return (updatedAccount, interest, hasEnoughMoney)

main :: IO ()
main = do
    let cfg = BankConfig
            { interestRate = 0.05
            , transactionFee = 2
            , minimumBalance = 100
            }

    let acc1 = Account
            { accountId = "A-001"
            , balance = 1000
            }

    let acc2 = Account
            { accountId = "A-002"
            , balance = 80
            }

    print (runReader ask cfg)
    print (runReader (asks interestRate) cfg)
    print (runReader (calculateInterest acc1) cfg)
    print (runReader (applyTransactionFee acc1) cfg)
    print (runReader (checkMinimumBalance acc1) cfg)
    print (runReader (processAccount acc1) cfg)
    print (runReader (processAccount acc2) cfg)
    print (runReader (local (\c -> c { transactionFee = 10 }) (applyTransactionFee acc1)) cfg)
