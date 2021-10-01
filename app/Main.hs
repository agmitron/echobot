{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Internal as BC
import GHC.Generics
import Network.HTTP.Simple (Request, getResponseBody, httpBS, parseRequest_)

newtype Chat = Chat
  { id :: Integer
  }
  deriving (Show, Generic)

data Message = Message
  { text :: String,
    chat :: Chat
  }
  deriving (Show, Generic)

data Result = Result
  { message :: Message,
    update_id :: Integer
  }
  deriving (Show, Generic)

data Response = Response
  { ok :: Bool,
    result :: Results
  }
  deriving (Show, Generic)

type Results = [Result]

instance FromJSON Chat
instance ToJSON Chat

instance FromJSON Message
instance ToJSON Message

instance FromJSON Result
instance ToJSON Result

instance FromJSON Response
instance ToJSON Response

-- todo: move token to .env
token = "<token>"

endpoint = "https://api.telegram.org/bot" ++ token ++ "/"

getResponse :: BC.ByteString -> Either String Response
getResponse = eitherDecode

getUpdateId :: Maybe Result -> Maybe Integer
getUpdateId (Just res) = Just (update_id res)
getUpdateId Nothing = Nothing


main :: IO ()
main = do
  response <- httpBS (parseRequest_ (endpoint ++ "getUpdates"))
  B8.putStrLn $ B8.pack $ show $ getResponse $ fromStrict $ getResponseBody response
