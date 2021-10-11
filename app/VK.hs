{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VK where

import System.Random
import Data.Aeson hiding (object)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Internal as BC
import GHC.Generics
import Network.HTTP.Simple (Request, getResponseBody, httpBS, parseRequest_, setRequestBodyJSON, Response, setRequestBodyURLEncoded)
import Data.ByteString.Lazy.Char8 (fromStrict)
import Data.Maybe (isNothing, isJust)
import Data.Functor
import qualified Control.Monad.IO.Class


safeHead [] = Nothing
safeHead (x : _) = Just x

type VKGroupID = Integer

type VKGroupAccessToken = String

type VKAPIVersion = String

data GetLongPollRequest = GetLongPollRequest
  { vkGroupId :: VKGroupID,
    vkGroupAccessToken :: VKGroupAccessToken,
    vkAPIVersion :: VKAPIVersion
  }

type VKLongPollSessionKey = String

type VKLongPollServerAddress = String

type VKLongPollLastUpdateID = String

data GetLongPollResponseObject = GetLongPollResponseObject
  { vkLongPollSessionKey :: VKLongPollSessionKey,
    vkLongPollServerAddress :: VKLongPollServerAddress,
    vkLongPollLastUpdateID :: VKLongPollLastUpdateID
  } deriving (Show, Generic)

newtype GetLongPollResponse = GetLongPollResponse
  {
    response :: GetLongPollResponseObject
  } deriving (Show, Generic)

instance FromJSON GetLongPollResponse

instance FromJSON GetLongPollResponseObject where
  parseJSON (Object v) = do
    key <- v .: "key"
    server <- v .: "server"
    tmstmp <- v .: "ts"
    pure GetLongPollResponseObject {
      vkLongPollSessionKey = key
      , vkLongPollServerAddress = server
      , vkLongPollLastUpdateID = tmstmp
    }

data VKMessage = VKMessage
  {
    resp_peer_id :: Integer
    , text :: String
    , from_id :: Integer
  }  deriving (Show, Generic)

instance FromJSON VKMessage where
  parseJSON (Object v) =
    VKMessage <$> v .: "peer_id"
      <*> v .: "text"
      <*> v .: "from_id"
instance ToJSON VKMessage

newtype VKUpdateObject = VKUpdateObject
  { msg :: VKMessage
  }  deriving (Show, Generic)

instance FromJSON VKUpdateObject where
  parseJSON (Object v) =
    VKUpdateObject <$> v .: "message"

data VKUpdate = VKUpdate
  { update_type :: String
    , object :: VKUpdateObject
    , event_id :: String
  } deriving (Show, Generic)

instance FromJSON VKUpdate where
  parseJSON (Object v) = do
    upd_type <- v .: "type"
    upd_obj <- v .: "object"
    eid <- v .: "event_id"
    pure $ VKUpdate {
      update_type = upd_type
      , object = upd_obj
      , event_id = eid
    }



data VKUpdateResponse = VKUpdateResponse
  { new_ts :: String,
    updates :: [VKUpdate]
  } deriving (Show, Generic)

instance FromJSON VKUpdateResponse where
  parseJSON (Object v) = do
    nts <- v .: "ts"
    updts <- v .: "updates"
    pure $ VKUpdateResponse {new_ts = nts, updates = updts}

host = "https://api.vk.com/method/"

createEndpoint :: String -> String
createEndpoint endpoint = host ++ endpoint

createParams :: [(String, String)] -> String
createParams = foldl (\acc (key, value) -> acc ++ key ++ "=" ++ value ++ "&") "?"

getLongPollServer :: GetLongPollRequest -> IO (Maybe GetLongPollResponseObject)
getLongPollServer (GetLongPollRequest vkGroupId vkGroupAccessToken vkAPIVersion) = do
  let request = parseRequest_ $ createEndpoint "groups.getLongPollServer" ++ createParams [("group_id", show vkGroupId), ("access_token", vkGroupAccessToken), ("v", vkAPIVersion)]
  res <- httpBS request
  let json = getResponseBody res
  let lpserverData = decode $ fromStrict json
  case lpserverData of
    Nothing -> return Nothing
    Just d ->  return (Just $ response d)


data CheckForUpdatesRequest = CheckForUpdatesRequest
  { server :: String,
    key :: String,
    ts :: String,
    wait :: Integer
  }

checkForUpdates :: CheckForUpdatesRequest -> IO B8.ByteString
checkForUpdates (CheckForUpdatesRequest server key ts wait) = do
  let requestURL = server ++ "?act=a_check&key=" ++ key ++ "&ts=" ++ ts ++ "&wait=" ++ show wait
  response <- httpBS $ parseRequest_ requestURL
  return (getResponseBody response)

data SendMessageRequestBody = SendMessageRequestBody
  { message :: String,
    user_id :: Integer,
    peer_id :: Integer,
    access_token :: String,
    random_id :: Int
  }
  deriving (Show, Generic)

instance ToJSON SendMessageRequestBody

sendMessage :: Control.Monad.IO.Class.MonadIO m => [(B8.ByteString, B8.ByteString)] -> m (Response B8.ByteString)
sendMessage requestBody = httpBS request
  where
    body = setRequestBodyURLEncoded requestBody
    request = body $ parseRequest_ $ createEndpoint "messages.send"

-- runVKBot :: IO ()
-- runVKBot = getLongPollServer $ GetLongPollRequest {
--   vkGroupId = 0,
--   vkGroupAccessToken = "",
--   vkAPIVersion = ""
-- }

-- runVKBot :: IO ()
-- runVKBot = checkForUpdates $ CheckForUpdatesRequest {
--   server = "https://lp.vk.com/wh207736680",
--   ts = "7",
--   key = "e33dae2ee52a773ec87e066a363cdd0746ace581",
--   wait = 25
-- }

parseVkUpdate :: BC.ByteString -> Maybe VKUpdateResponse
parseVkUpdate = decode

getNecessaryReplyData :: Maybe VKUpdateResponse -> Maybe (Int -> [(B8.ByteString, B8.ByteString)])
getNecessaryReplyData Nothing = Nothing
getNecessaryReplyData (Just response) = do
      let upd = safeHead $ updates response
      m <- upd <&> text . msg . object
      uid <- upd <&> from_id . msg . object
      pid <- upd <&> resp_peer_id . msg . object
      return
        ( \rid -> [
          ("message", B8.pack m),
          ("user_id", B8.pack $ show uid),
          ("peer_id", B8.pack $ show pid),
          ("access_token", ""),
          ("random_id", B8.pack $ show rid),
          ("v", "5.131")
        ])

runVKBot :: Maybe StdGen ->  IO ()
runVKBot Nothing = do
  lpserverData <- getLongPollServer $ GetLongPollRequest {
    vkGroupId = 0,
    vkGroupAccessToken = "",
    vkAPIVersion = "5.131"
  }
  case lpserverData of
    Nothing -> error "Incorrect Long Poll Server data."
    Just lpsd -> do
      json <- checkForUpdates $
        CheckForUpdatesRequest
          { server = vkLongPollServerAddress lpsd,
            ts = vkLongPollLastUpdateID lpsd,
            key = vkLongPollSessionKey lpsd,
            wait = 25
          }
      randomGen <- getStdGen
      let (rid, newGen) = randomR (0, 2147483647) randomGen :: (Int, StdGen)
      case getNecessaryReplyData $ parseVkUpdate (fromStrict json) of
        Nothing -> do
          runVKBot Nothing
        Just msgRequestData -> do
          response <- sendMessage $ msgRequestData rid
          putStrLn $ B8.unpack $ getResponseBody response
          runVKBot (Just newGen)

runVKBot (Just gen) = do
  lpserverData <- getLongPollServer $ GetLongPollRequest {
    vkGroupId = 207736680,
    vkGroupAccessToken = "f60b905200501782ab5b3f3faaed5661703dbf9f25a76b3df8edf68576acb52d7e1b8899c6b1c6aef78f3",
    vkAPIVersion = "5.131"
  }
  case lpserverData of
    Nothing -> error "Incorrect Long Poll Server data."
    Just lpsd -> do
      json <- checkForUpdates $
        CheckForUpdatesRequest
          { server = vkLongPollServerAddress lpsd,
            ts = vkLongPollLastUpdateID lpsd,
            key = vkLongPollSessionKey lpsd,
            wait = 25
          }
      let (rid, newGen) = randomR (0, 2147483647) gen :: (Int, StdGen)
      case getNecessaryReplyData $ parseVkUpdate (fromStrict json) of
        Nothing -> do
          runVKBot Nothing
        Just msgRequestData -> do
          response <- sendMessage $ msgRequestData rid
          putStrLn $ B8.unpack $ getResponseBody response
          runVKBot (Just newGen)
