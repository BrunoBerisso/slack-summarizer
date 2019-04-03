{-# LANGUAGE OverloadedStrings #-}
module Lib (getMessages) where

import qualified Data.ByteString as BS
import Data.Aeson (Value)
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS

type ChannelId = BS.ByteString
data Message = Message String String

setupRequestManager :: IO ()
setupRequestManager = do
    manager <- newManager tlsManagerSettings
    setGlobalManager manager

-- "xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb"
-- "C5UJEF537"

getMessages :: ChannelId -> IO ()
getMessages channelId = do
    let request = getRequest channelId "xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb"
    response <- httpJSON request
    putStrLn $ show (getResponseBody response :: Value)
    where
        getRequest channel token = setRequestHost "slack.com"
                                    $ setRequestPath "/api/channels.history"
                                    $ setRequestMethod "GET"
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ setRequestQueryString [("token", Just token), ("channel", Just channel)]
                                    $ defaultRequest

