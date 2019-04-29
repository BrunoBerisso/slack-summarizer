{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway
import Network.HTTP.Simple
import Control.Lens
import qualified Data.Map.Strict as Map
import Data.ByteString
import Data.Maybe (fromMaybe)
import Control.Monad (join)

import Lib

data SummarizeParams = SummarizeParams {
  responseUrl :: ByteString,
  channelId :: ByteString -- C5UJEF537
} deriving(Show)

parseQueryParams :: Query -> Maybe SummarizeParams
parseQueryParams query =
  let
    maybeResponseUrl = flatten $ Map.lookup "response_url" params
    maybeChannelId = flatten $ Map.lookup "channel_id" params
  in
    maybeResponseUrl >>= (\responseUrl -> maybeChannelId >>= (\channelId -> Just $ SummarizeParams responseUrl channelId))
  where
    params = Map.fromList query
    flatten = join

main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
handler request = do
  let query = request ^. agprqQueryStringParameters
  let body = request ^. requestBody
  Prelude.putStrLn $ "Did receive query: " ++ (show query) ++ " With body: " ++ (show body)
  params <- case parseQueryParams query of
    Just r -> return r
    Nothing -> fail "Can't read query string"
  messages <- getMessages (channelId params)
  summary <- summarizeMessages messages
  print summary
  pure $ responseOK & responseBodyEmbedded ?~ [1, 2, 3]
