--module Main where

--import Lib

--main :: IO ()
--main = someFunc

--websockets example
-- ==================

-- This is the Haskell implementation of the example for the WebSockets library. We
-- implement a simple multi-user chat program. A live demo of the example is
-- available [here](/example/client.html).  In order to understand this example,
-- keep the [reference](/reference/) nearby to check out the functions we use.

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

-- new TestTyp
type Clients = [WS.Connection]

-- newClient
newClients :: Clients
newClients = []
-- Get the number of active clients:


-- new addClient
addClient_ :: WS.Connection -> Clients -> Clients
addClient_ conn clients = conn : clients

main :: IO ()
main = do
--    state <- newMVar newServerState
    T.putStrLn "Server started."
    clients <- newMVar newClients
    WS.runServer "127.0.0.1" 9160 $ application clients

-- Our main application has the type:

application :: MVar Clients -> WS.ServerApp

application clients pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
      modifyMVar_ clients $ \c -> do
        let cl = addClient_ conn c
        T.putStrLn "Client Connected."
        return cl
      WS.sendTextData conn ("Client connected" :: Text)
      talk conn clients

talk :: WS.Connection -> MVar Clients -> IO ()
talk conn clients = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn (T.append (T.pack "Client -> Server : ") msg)
  T.putStrLn (T.append (T.pack "Client <- Server : ") msg)
  WS.sendTextData conn (msg :: Text)
