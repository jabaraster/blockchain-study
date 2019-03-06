module BlockChain
    ( Transaction(..)
    , Block
    , BlockChain
    )
where

import           Data.Time.Clock
import           Data.Time.Calendar

type Index = Integer
type Proof = Integer
type Hash = String

data Transaction = Transaction
    { sender :: String
    , recipient :: String
    , amount :: Integer
    } deriving (Show)

data Block = Block
    { index :: Index
    , timestamp :: UTCTime
    , proof :: Proof
    , previousHash :: Hash
    , transactions :: [Transaction]
    } deriving (Show)

data BlockChain = BlockChain
    { chain :: [Block]
    , currentTransactions :: [Transaction]
    }

genesisBlock :: UTCTime -> Block
genesisBlock t = Block { index        = 0
                       , timestamp    = t
                       , proof        = 100
                       , previousHash = "1"
                       , transactions = []
                       }

def :: IO BlockChain
def = getCurrentTime >>= \t ->
    pure BlockChain { chain = [genesisBlock t], currentTransactions = [] }

newTransaction :: Transaction -> BlockChain -> BlockChain
newTransaction t bc = bc { currentTransactions = currentTransactions bc ++ [t] }

newBlock :: Proof -> Hash -> BlockChain -> IO BlockChain
newBlock p h bc = getCurrentTime >>= \t ->
    let b = Block { index        = ((+) 1) $ fromIntegral $ length $ chain bc
                  , timestamp    = t
                  , proof        = p
                  , previousHash = h
                  , transactions = currentTransactions bc
                  }
    in  pure bc { chain = chain bc ++ [b], currentTransactions = [] }
