{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (id)
import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Internal as BC
import GHC.Generics
import Network.HTTP.Simple (Request, getResponseBody, httpBS, parseRequest_, Response)
import Data.Function ((&))
import Data.Either (fromRight)
import qualified Control.Monad.IO.Class

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

getResponse :: BC.ByteString -> Either String TelegramResponse
getResponse = eitherDecode

getUpdateId :: Either String TelegramResponse -> Either String Integer
getUpdateId (Right res) = Right (update_id $ head $ result res)
getUpdateId (Left err) = Left err

incrementOffset :: Integer -> Integer
incrementOffset offset = offset + 1

getChatIdAndMessageText :: TelegramResponse -> (Integer, String)
getChatIdAndMessageText res = (chatId, messageText)
  where
    msg = message $ head (result res)
    chatId = msg & chat & id
    messageText = msg & text


sendMessage (chatId, messageText) = do
  httpBS (parseRequest_ (endpoint ++ "sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ show messageText))

main :: IO ()
main = do
  responseJSON <- httpBS (parseRequest_ (endpoint ++ "getUpdates"))
  let updatesResponse = getResponse $ fromStrict $ getResponseBody responseJSON
  case updatesResponse of
        (Left err) -> do putStrLn err
        (Right response) -> do
          responseJSON <- sendMessage (getChatIdAndMessageText response)
          B8.putStrLn $ getResponseBody responseJSON
