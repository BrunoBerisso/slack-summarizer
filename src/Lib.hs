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

type ChannelId = ByteString

data Message = Message {
    user :: String,
    text :: String
} deriving (Generic, Show)

instance FromJSON Message

data SummarizeParams = SummarizeParams {
  responseUrl :: ByteString,
  channelId :: ChannelId, -- C5UJEF537
  messageCount :: Integer
} deriving(Show)

parseQueryParams :: Query -> Maybe SummarizeParams
parseQueryParams query =
  let
    maybeResponseUrl = flatten $ Map.lookup "response_url" params
    maybeChannelId = flatten $ Map.lookup "channel_id" params
    maybeMessageCount =  (flatten $ Map.lookup "text" params) >>= fromByteString
  in
    maybeResponseUrl >>= 
      (\responseUrl -> maybeChannelId >>= 
        (\channelId -> maybeMessageCount >>=
          (\messageCount -> 
            Just $ SummarizeParams responseUrl channelId messageCount)))
  where
    params = Map.fromList query
    flatten = join

{-
REQUESTS:
This are the requests performed to get the messages and summiraze the text
-}

slackAccountToken = "xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb"

-- | Construct a Request that, when performed, returns the more resent messages in the channel how ChannelId is passed as argument
getSlackChannelHistory :: ChannelId -> Integer -> Request
getSlackChannelHistory channel count = setRequestHost "slack.com"
                                $ setRequestPath "/api/channels.history"
                                $ setRequestMethod "GET"
                                $ setRequestSecure True
                                $ setRequestPort 443
                                $ setRequestQueryString [
                                    ("token", Just slackAccountToken),
                                    ("channel", Just channel),
                                    ("count", Just $ Char8.pack $ show count)]
                                $ defaultRequest

algorithmiaKey = "Simple simi/k2XRHwTjswcXf4NKuv7NVP1"

summarizeText text = setRequestHost "api.algorithmia.com"
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
    response <- httpJSON $ getSlackChannelHistory (channelId params) (messageCount params)
    messages <- case parseResponseBody response of
        Right m -> return m
        Left e  -> fail e
    return messages
    where
        parseResponseBody response = let body = getResponseBody response
                                    in parseEither (.: "messages") body 

summarizeMessages :: [Message] -> IO (String)
summarizeMessages messages = do
    let paragraph = reduceToParagraph messages
    response <- httpJSON $ summarizeText paragraph
    text <- case parseResponseBody response of
        Right t -> return t
        Left e -> fail e
    return text
    where
        parseResponseBody response = let body = getResponseBody response
                                    in parseEither (.: "result") body
        reduceToParagraph = Prelude.foldl (\paragraph message -> paragraph ++ (user message) ++ (text message)) ""

