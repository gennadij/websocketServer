{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace, isDigit)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Lib ( calcExactRoot )
import Data.Aeson
import GHC.Generics

-- new TestTyp
type Clients = [WS.Connection]

-- newClient
newClients :: Clients
newClients = []
-- Get the number of active clients:


-- new addClient
addClient :: WS.Connection -> Clients -> Clients
addClient conn clients = conn : clients

-- Test Json
data ExactRoot = ExactRoot{
      action :: !Text,
      device :: !Text
} deriving (Show, Generic)

instance FromJSON ExactRoot
instance ToJSON ExactRoot

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
        let cl = addClient conn c
        T.putStrLn "Client Connected."
        return cl
      WS.sendTextData conn ("Client connected" :: Text)
      talk conn clients

talk :: WS.Connection -> MVar Clients -> IO ()
talk conn clients = forever $ do  
  msg <- WS.receiveData conn
  let Just res = decode (WS.toLazyByteString msg) :: Maybe ExactRoot
  print (action res)
  print (device res)
  -- putStrLn $ "Decode: " ++ (show (decode $ WS.toLazyByteString msg :: Maybe CalcExactRoot))
  handleMsg msg conn
  -- T.putStrLn (T.append (T.pack "Client -> Server : ") msg)
  -- WS.sendTextData conn (
    -- T.pack "Response: " `T.append` T.pack (
      -- calcExactRoot(convertTextToInt (msg :: Text))))
    

convertTextToInt :: Text -> Int 
convertTextToInt st = read (T.unpack st) :: Int

handleMsg :: Text -> WS.Connection -> IO ()
handleMsg msg conn = case msg of
  _ | isNummeric $ T.unpack msg -> WS.sendTextData conn (T.pack "Response: " `T.append` T.pack (calcExactRoot(convertTextToInt (msg :: Text))))
    | otherwise -> WS.sendTextData conn (encode (ExactRoot{action = "response", device = "test2"}))

isNummeric :: String -> Bool
isNummeric [] = False
isNummeric xs = all isDigit xs

