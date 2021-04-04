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
addClient :: WS.Connection -> Clients -> Clients
addClient conn clients = conn : clients

main :: IO ()
main = do
<<<<<<< HEAD
    state <- newMVar newServerState
    T.putStrLn "Websocket Server run"
    WS.runServer "127.0.0.1" 9160 $ application state

-- Our main application has the type:

application :: MVar ServerState -> WS.ServerApp

-- Note that `WS.ServerApp` is nothing but a type synonym for
-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
-- application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.

application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) $ do

-- When a client is succesfully connected, we read the first message. This should
-- be in the format of "Hi! I am Jasper", where Jasper is the requested username.

        msg <- WS.receiveData conn
        clients <- readMVar state
        T.putStrLn ("Message from Client : " <> msg)
        case msg of

-- Check that the first message has the right format:
            _   | False -> T.putStrLn "False"   -- WS.sendTextData conn (msg :: Text)
            -- _   | not (prefix `T.isPrefixOf` msg) ->
              --      WS.sendTextData conn ("Wrong announcement" :: Text)

-- Check the validity of the username:

              --  | any ($ fst client)
              --      [T.null, T.any isPunctuation, T.any isSpace] ->
              --          WS.sendTextData conn ("Name cannot " <>
              --              "contain punctuation or whitespace, and " <>
              --              "cannot be empty" :: Text)

-- Check that the given username is not already taken:

              --  | clientExists client clients ->
              --      WS.sendTextData conn ("User already exists" :: Text)

-- All is right! We're going to allow the client, but for safety reasons we *first*
-- setup a `disconnect` function that will be run when the connection is closed.

                | otherwise -> flip finally disconnect $ do

-- We send a "Welcome!", according to our own little protocol. We add the client to
-- the list and broadcast the fact that he has joined. Then, we give control to the
-- 'talk' function.
                      WS.sendTextData conn msg
                   --modifyMVar_ state $ \s -> do
                       --let s' = addClient client s
                       --WS.sendTextData conn $
                           --"Welcome! Users: " <>
                           -- T.intercalate ", " (map fst s)
                       -- broadcast (fst client <> " joined") s'
                       -- return s'
                    -- talk client state
             where
               prefix     = "Hi! I am "
               client     = (T.drop (T.length prefix) msg, conn)
               disconnect = do
                   -- Remove client and return new state
                   s <- modifyMVar state $ \s ->
                       let s' = removeClient client s in return (s', s')
                   broadcast (fst client <> " disconnected") s

-- The talk function continues to read messages from a single client until he
-- disconnects. All messages are broadcasted to the other clients.

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
=======
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
        let cl = addClient conn c
        T.putStrLn "Client Connected."
        return cl
      WS.sendTextData conn ("Client connected" :: Text)
      talk conn clients

talk :: WS.Connection -> MVar Clients -> IO ()
talk conn clients = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn (T.append (T.pack "Client -> Server : ") msg)
  T.putStrLn (T.append (T.pack "Client <- Server : ") msg)
  WS.sendTextData conn (T.pack "Response: " `T.append` msg  :: Text)
>>>>>>> origin/testProjekt_3
