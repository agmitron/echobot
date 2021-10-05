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
token = "1975796426:AAGiifTuCe5-PN25Uzc28omSHFxA37GmphI"

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


sendMessage (chatId, messageText) =
  httpBS (parseRequest_ (endpoint ++ "sendMessage?chat_id=" ++ show chatId ++ "&text=" ++ show messageText))

reply :: Response B8.ByteString -> IO (Either String TelegramResponse)
reply responseJSON = do
  let updatesResponse = getResponse responseJSON
  case updatesResponse of
        (Left err) -> putStrLn err
        (Right response) -> do
          responseJSON <- sendMessage (getChatIdAndMessageText response)
          B8.putStrLn $ getResponseBody responseJSON
  return updatesResponse

checkAndReply :: Maybe Integer -> IO ()
checkAndReply offset = do
  responseJSON <- case offset of
    Nothing -> httpBS (parseRequest_ (endpoint ++ "getUpdates"))
    Just x -> httpBS (parseRequest_ (endpoint ++ "getUpdates" ++ "?offset=" ++ show x))
  updatesResponse <- reply responseJSON
  case getUpdateId $ getResponse responseJSON of
    Left err -> error err
    Right updateId ->
      checkAndReply (Just $ incrementOffset updateId)

main :: IO ()
main =
  checkAndReply Nothing

