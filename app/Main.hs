{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import qualified ExactRoot as ER ( berechneWurzel, Ergebnis ( .. )) 
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as LB

-- new TestTyp
type Clients = [WS.Connection]

-- newClient
newClients :: Clients
newClients = []
-- Get the number of active clients:

-- new addClient
addClient :: WS.Connection -> Clients -> Clients
addClient conn clients = conn : clients

data RequestJson = RequestJson{
  requestAction :: String,
  requestData :: JsonData
} deriving (Show, Generic, FromJSON, ToJSON)

data JsonData = JsonData {
  radicand :: Int,
  resExactRootMultiplier :: String,
  resExactRootSqrt :: String
} deriving (Show, Generic, FromJSON, ToJSON)

data ResponseJson = ResponseJson {
  responseAction :: String,
  responseData :: JsonData
} deriving (Show, Generic, FromJSON, ToJSON)

-- instance FromJSON ExactRoot
-- instance ToJSON ExactRoot

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
      -- WS.sendTextData conn ("Client connected" :: Text)
      talk conn clients

talk :: WS.Connection -> MVar Clients -> IO ()
talk conn clients = forever $ do
  msg <- WS.receiveData conn
  -- print (decodeJson msg)
  print msg
  handleMsg (decodeJson msg) conn

convertTextToInt :: Text -> Int
convertTextToInt st = read (T.unpack st) :: Int

handleMsg :: Maybe RequestJson -> WS.Connection -> IO ()
handleMsg jObject conn = case jObject of
  Just obj | isExactRoot $ requestAction obj -> WS.sendTextData conn (convertToJsonResponse (radicand (requestData obj)))
           | otherwise -> WS.sendTextData conn ("Unbekannte Anfrage" :: Text)
  Nothing -> WS.sendTextData conn ("Unbekante Anfrage" :: Text)

isNummeric :: String -> Bool
isNummeric [] = False
isNummeric xs = all isDigit xs

-- execExactRootString :: Int -> String
-- execExactRootString = calcExactRootString

convertToJsonResponse :: Int -> LB.ByteString
convertToJsonResponse radicand = encode (
  ResponseJson {
    responseAction = "exactRoot",
    responseData = JsonData {
      radicand = 0,
      resExactRootMultiplier = multiplier,
      resExactRootSqrt = sqrt
    }
  })
  where 
    multiplier = getMuliplier exactRoot
    sqrt = getSqrt exactRoot
    exactRoot = execExactRoot radicand


isExactRoot :: String -> Bool
isExactRoot action =
  action == "exactRoot"

decodeJson :: Text -> Maybe RequestJson
decodeJson msg = decode (WS.toLazyByteString msg) :: Maybe RequestJson

execExactRoot :: Int -> ER.Ergebnis
execExactRoot = ER.berechneWurzel

getMuliplier :: ER.Ergebnis -> String
getMuliplier e = show (ER.multiplikator e)

getSqrt :: ER.Ergebnis -> String
getSqrt e = show (ER.wurzelWert e) 
