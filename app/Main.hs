{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Internal as BC
import Data.Either (fromRight)
import Data.Function ((&))
import GHC.Generics
import Network.HTTP.Simple (Request, Response, getResponseBody, httpBS, parseRequest_)
import Prelude hiding (id)
import qualified Prelude as P (id)

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

data TelegramResponse = TelegramResponse
  { ok :: Bool,
    result :: [Result]
  }
  deriving (Show, Generic)

instance FromJSON Chat

instance ToJSON Chat

instance FromJSON Message

instance ToJSON Message

instance FromJSON Result

instance ToJSON Result

instance FromJSON TelegramResponse

instance ToJSON TelegramResponse

-- todo: move token to .env
token = ""

endpoint = "https://api.telegram.org/bot" ++ token ++ "/"

getResponse :: Response B8.ByteString -> Either String TelegramResponse
getResponse responseJSON = eitherDecode $ fromStrict $ getResponseBody responseJSON

getUpdateId :: Either String TelegramResponse -> Either String Integer
getUpdateId (Right res) = if null results then Left "Results array is empty" else Right (update_id $ head results)
  where
    results = result res
getUpdateId (Left err) = Left $ "getUpdateId error: " ++ err

incrementOffset :: Integer -> Integer
incrementOffset offset = offset + 1

getChatIdAndMessageText :: TelegramResponse -> (Integer, String)
getChatIdAndMessageText res = (chatId, messageText)
  where
    msg = message $ head (result res)
    chatId = msg & chat & id
    messageText = msg & text

sendMessage :: (Control.Monad.IO.Class.MonadIO m) => (Integer, String) -> m ()
sendMessage (chatId, messageText) = do
  httpBS (parseRequest_ (endpoint ++ "sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ P.id messageText))
  return ()

reply :: Response B8.ByteString -> IO (Either String TelegramResponse)
reply responseJSON = do
  let updatesResponse = getResponse responseJSON
  case updatesResponse of
    (Left err) -> putStrLn err
    (Right response) ->
      if null (result response)
        then return ()
        else sendMessage $ getChatIdAndMessageText response
  return updatesResponse

checkAndReply :: Maybe Integer -> IO ()
checkAndReply offset = do
  responseJSON <- case offset of
    Nothing -> httpBS (parseRequest_ (endpoint ++ "getUpdates"))
    Just x -> httpBS (parseRequest_ (endpoint ++ "getUpdates" ++ "?offset=" ++ show x))
  updatesResponse <- reply responseJSON
  case getUpdateId $ getResponse responseJSON of
    Left _ -> checkAndReply Nothing
    Right updateId ->
      checkAndReply (Just $ incrementOffset updateId)

main :: IO ()
main =
  checkAndReply Nothing
