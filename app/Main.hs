{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.HTTP.Simple (httpBS, getResponseBody)
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = httpBS "http://example.com" >>= B8.putStrLn . getResponseBody
