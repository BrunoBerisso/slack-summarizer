{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (getMessages) where

import GHC.Generics
import Data.ByteString.Char8 (pack)
import Data.Aeson (FromJSON, Value, Object, (.:))
import Data.Aeson.Types
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS

type ChannelId = String

data Message = Message {
    user :: String,
    text :: String
} deriving (Generic, Show)

instance FromJSON Message

setupRequestManager :: IO ()
setupRequestManager = do
    manager <- newManager tlsManagerSettings
    setGlobalManager manager

-- "xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb"
-- "C5UJEF537"

slackAccountToken = "xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb"

getMessages :: ChannelId -> IO ([Message])
getMessages channelId = do
    let request = getRequest (pack channelId) slackAccountToken
    response <- httpJSON request
    messages <- case parseResponseBody response of
        Right m  -> return m
        Left e -> fail e
    putStrLn $ show messages
    return messages
    where
        getRequest channel token = setRequestHost "slack.com"
                                    $ setRequestPath "/api/channels.history"
                                    $ setRequestMethod "GET"
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ setRequestQueryString [("token", Just token), ("channel", Just channel), ("count", Just "5")]
                                    $ defaultRequest
        parseResponseBody response = let body = getResponseBody response
                                    in parseEither (.: "messages") body 

