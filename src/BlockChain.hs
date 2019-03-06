{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BlockChain where

import qualified Crypto.Hash.SHA256            as S
import           Data.Aeson
import qualified Data.ByteString               as BB
                                                ( ByteString )
import qualified Data.ByteString.Base16        as B16
                                                ( encode )
import qualified Data.ByteString.Char8         as B
                                                ( index
                                                , length
                                                , pack
                                                , unpack
                                                )
import           Data.Time.Clock
import           GHC.Generics

type Index = Integer
type Proof = Integer
type Hash = String

data Transaction = Transaction
    { sender    :: String
    , recipient :: String
    , amount    :: Integer
    } deriving (Generic, Show, FromJSON, ToJSON)

data Block = Block
    { index        :: Index
    , timestamp    :: UTCTime
    , proof        :: Proof
    , previousHash :: Hash
    , transactions :: [Transaction]
    } deriving (Generic, Show, ToJSON)

data BlockChain = BlockChain
    { chain               :: [Block]
    , currentTransactions :: [Transaction]
    } deriving (Generic)

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
newTransaction t bc =
    bc { currentTransactions = currentTransactions bc ++ [t] }

newBlock :: Proof -> Hash -> UTCTime -> BlockChain -> BlockChain
newBlock p h t bc =
    let b = Block { index        = fromIntegral $ length $ chain bc
                  , timestamp    = t
                  , proof        = p
                  , previousHash = h
                  , transactions = currentTransactions bc
                  }
    in  bc { chain = chain bc ++ [b], currentTransactions = [] }

blockAt :: Int -> BlockChain -> Maybe Block
blockAt idx bc =
    let c = chain bc
    in if idx >= length c
          then Nothing
          else Just $ c !! idx

lastBlock :: BlockChain -> Block
lastBlock bc =
    let c = chain bc
    in  if null c
            then error "Genesis Block is not created!!"
            else c !! ((length c) - 1)

blockToHash :: Block -> Hash
blockToHash = hash . show

hash :: String -> Hash
hash = B.unpack . B16.encode . S.hash . B.pack

proofOfWork :: Hash -> Proof -> Proof
proofOfWork preHash preProof = core 1
  where
    core :: Integer -> Proof
    core val =
        let p = preHash ++ show (preProof * val)
        in  if valid p then val else core (val + 1)

    valid :: String -> Bool
    valid p =
        let s = hash p
        in  length s
                >= 4
                && take 4 s == "0000"