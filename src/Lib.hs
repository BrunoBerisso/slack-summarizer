module Lib (getMessages) where

import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS

someFunc :: IO ()
someFunc = putStrLn "Serverless is running your lambda function!"


getMessages :: IO ()
getMessages = do
    manager <- newManager tlsManagerSettings
    setGlobalManager manager
    request <- parseRequest "GET https://slack.com/api/channels.history?token=xoxp-198580037300-198456467459-200151962098-c14a436eefc7d812886e53b6786a18eb&channel=C5UJEF537&pretty=1"
    response <- httpJSON request
    putStrLn $ getResponseBody response
