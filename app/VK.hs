{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module VK where
import Network.HTTP.Simple (httpBS, Request, parseRequest_, getResponseBody)
import qualified Data.ByteString.Char8 as B8

type VKGroupID = Integer
type VKGroupAccessToken = String
type VKAPIVersion = String

data GetLongPollRequest = GetLongPollRequest {
  vkGroupId :: VKGroupID
  , vkGroupAccessToken :: VKGroupAccessToken
  , vkAPIVersion :: VKAPIVersion
}

type VKLongPollSessionKey = String
type VKLongPollServerAddress = String
type VKLongPollLastUpdateID = String

data GetLongPollResponse = GetLongPollResponse {
  vkLongPollSessionKey :: VKLongPollSessionKey
  , vkLongPollServerAddress :: VKLongPollServerAddress
  , vkLongPollLastUpdateID :: VKLongPollLastUpdateID
}

host = "https://api.vk.com/method/"

createEndpoint :: String -> String
createEndpoint endpoint = host ++ endpoint

createParams :: [(String, String)] -> String
createParams = foldl (\acc (key, value) -> acc ++ key ++ "=" ++ value ++ "&") "?"

-- Maybe GetLongPollResponse
getLongPollServer :: GetLongPollRequest -> IO ()
getLongPollServer (GetLongPollRequest vkGroupId vkGroupAccessToken vkAPIVersion) = do
  let request = parseRequest_ $ createEndpoint "groups.getLongPollServer" ++ createParams [("group_id", show vkGroupId), ("access_token", vkGroupAccessToken), ("v", vkAPIVersion)]
  response <- httpBS request
  B8.putStrLn $ getResponseBody response
  putStrLn $ createEndpoint "groups.getLongPollServer" ++ createParams [("group_id", show vkGroupId), ("access_token", vkGroupAccessToken), ("v", vkAPIVersion)]


data CheckForUpdatesRequest = CheckForUpdatesRequest {
  server :: String,
  key :: String,
  ts :: String,
  wait :: Integer
}

checkForUpdates :: CheckForUpdatesRequest -> IO ()
checkForUpdates (CheckForUpdatesRequest server key ts wait) = do
  let requestURL = server ++ "?act=a_check&key=" ++ key ++ "&ts=" ++ ts ++ "&wait=" ++ show wait
  response <- httpBS $ parseRequest_ requestURL
  B8.putStrLn $ getResponseBody response
  putStrLn requestURL

-- runVKBot :: IO ()
-- runVKBot = getLongPollServer $ GetLongPollRequest {
--   vkGroupId = 0,
--   vkGroupAccessToken = "",
--   vkAPIVersion = ""
-- }

-- runVKBot :: IO ()
-- runVKBot = checkForUpdates $ CheckForUpdatesRequest {
--   server = "",
--   ts = "3",
--   key = ""
--   wait = 25
-- }
