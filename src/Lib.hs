{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (
    getMessages,
    summarizeMessages,
    SummarizeParams,
    parseQueryParams
) where

import GHC.Generics
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Conversion.From
import Data.Aeson (FromJSON, Value, Object, (.:))
import Data.Aeson.Types
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad (join)
import System.Environment (getEnv)

type ChannelId = ByteString

data Message = Message {
    text :: String
} deriving (Generic, Show)

instance FromJSON Message

data SummarizeParams = SummarizeParams {
  responseUrl :: ByteString,
  channelId :: ChannelId,
  messageCount :: Integer
} deriving(Show)

parseQueryParams :: Query -> Maybe SummarizeParams
parseQueryParams query = do
  responseUrl  <- flatten $ Map.lookup "response_url" params
  channelId    <- flatten $ Map.lookup "channel_id" params
  messageCount <- (flatten $ Map.lookup "text" params) >>= fromByteString
  return $ SummarizeParams responseUrl channelId messageCount
  where
    params = Map.fromList query
    flatten = join

{-
REQUESTS:
This are the requests performed to get the messages and summiraze the text
-}

-- | Construct a Request that, when performed, returns the more resent messages in the channel how ChannelId is passed as argument
getSlackChannelHistory slackAccountToken channel count = 
  setRequestHost "slack.com"
  $ setRequestPath "/api/channels.history"
  $ setRequestMethod "GET"
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestQueryString [
    ("token", Just slackAccountToken),
    ("channel", Just channel),
    ("count", Just . Char8.pack . show $ count)]
  $ defaultRequest

summarizeText algorithmiaKey text =
  setRequestHost "api.algorithmia.com"
  $ setRequestPath "/v1/algo/nlp/Summarizer/0.1.8"
  $ setRequestMethod "POST"
  $ setRequestSecure True
  $ setRequestPort 443
  $ setRequestHeader "Authorization" [algorithmiaKey]
  $ setRequestQueryString [("timeout", Just "300")]
  $ setRequestBodyJSON text
  $ defaultRequest

getMessages :: SummarizeParams -> IO ([Message])
getMessages params = do
  slackAccountToken <- getEnv "SLACK_ACCOUNT_TOKEN"
  response <- httpJSON $ getSlackChannelHistory (Char8.pack slackAccountToken) (channelId params) (messageCount params)
  messages <- case parseResponseBody response of
    Right m -> return m
    Left e  -> fail e
  return messages
  where
      parseResponseBody response = parseEither (.: "messages") $ getResponseBody response

summarizeMessages :: [Message] -> IO (String)
summarizeMessages messages = do
  algorithmiaKey <- getEnv "ALGORITHMIA_API_KEY"
  response  <- httpJSON $ summarizeText (Char8.pack algorithmiaKey) (reduceToParagraph messages)
  text      <- case parseResponseBody response of
    Right t -> return t
    Left e -> fail e
  return text
  where
      parseResponseBody response = parseEither (.: "result") $ getResponseBody response
      reduceToParagraph = Prelude.foldl (\paragraph message -> paragraph ++ (text message)) ""

