{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Data.Maybe
import           Data.Aeson
import           Network.Wai
import           Network.Wai.Logger
import           Network.Wai.Handler.Warp
import           Servant
import           Data.Time.Clock
import qualified BlockChain                    as BC
import           BlockChain                     ( BlockChain
                                                , Transaction(..)
                                                , Block
                                                , Hash
                                                )
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar as TV
import           Control.Monad.IO.Class
import           GHC.Generics


data GenericResponse = GenericResponse
  { message :: String
  } deriving (Generic, Show, ToJSON)

type API =      "static" :> Raw
           :<|> "transaction" :> "current" :> Get '[JSON] [Transaction]
           :<|> "transaction" :> ReqBody '[JSON] Transaction :> Post '[JSON] GenericResponse
           :<|> "mine" :> Post '[JSON] GenericResponse
           :<|> "block" :> Get '[JSON] [Block]
           :<|> "block" :> Capture "index" Int :> Get '[JSON] Block
           :<|> "block" :> Capture "index" Int :> "hash" :> Get '[JSON] Hash

api :: Proxy API
api = Proxy

err :: ServantErr -> Handler a
err = throwError

server :: TVar BlockChain -> Server API
server db = serveDirectoryFileServer "./static"
            :<|> transactions
            :<|> newTx
            :<|> mine
            :<|> blocks
            :<|> blockAt
            :<|> blockHashAt
  where
    blocks = liftIO $ do
                 bc <- readTVarIO db
                 pure $ BC.chain bc
    blockAt idx = do
                 m <- liftIO $ readTVarIO db >>= pure . BC.blockAt idx
                 case m of
                   Just b -> pure b
                   Nothing -> err err404
    blockHashAt idx = do
                 m <- liftIO $ readTVarIO db >>= \bc -> pure $ (BC.blockToHash <$> BC.blockAt idx bc)
                 case m of
                   Just b -> pure b
                   Nothing -> err err404
    transactions = liftIO $ do
                 bc <- readTVarIO db
                 pure $ BC.currentTransactions bc
    newTx tx = liftIO $ atomically $ do
                 bc <- readTVar db
                 writeTVar db $ BC.newTransaction tx bc
                 pure $ GenericResponse "OK"
    mine = liftIO $ do
                 bc <- readTVarIO db
                 let block = BC.lastBlock bc
                 let newProof = BC.proofOfWork (BC.blockToHash block) (BC.proof block)
                 let hash = BC.blockToHash block
                 let fee = Transaction
                              { sender    = "fee"
                              , recipient = "me"
                              , amount    = 1
                              }
                 now <- getCurrentTime
                 let newBc = BC.newBlock newProof hash now $ BC.newTransaction fee bc
                 atomically $ writeTVar db newBc
                 pure $ GenericResponse "OK"

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
  bc <- BC.def
  db <- atomically $ newTVar bc
  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings $ serve api $ server db