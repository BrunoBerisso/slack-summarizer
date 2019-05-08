{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway
import Network.HTTP.Simple
import Control.Lens

import Lib

data SummarizeResponse = SummarizeResponse {
  text :: String
} deriving(Generic, Show)

instance ToJSON SummarizeResponse

main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded SummarizeResponse))
handler request = do
  Prelude.putStrLn $ "Did receive query: " ++ (show query) ++ " With body: " ++ (show body)
  params <- case parseQueryParams query of
    Just r -> return r
    Nothing -> fail "Can't read query string"
  summary <- getMessages params >>= summarizeMessages
  print summary
  pure $ responseOK & responseBodyEmbedded ?~ (SummarizeResponse summary)
  where
    query = request ^. agprqQueryStringParameters
    body = request ^. requestBody
