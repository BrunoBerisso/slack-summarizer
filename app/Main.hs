module Main where

import Data.Aeson
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway
import Control.Lens

import Lib

main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded [Int]))
handler request = do
  putStrLn "This should go to logs"
  getMessages "C5UJEF537"
  print $ request ^. requestBody
  pure $ responseOK & responseBodyEmbedded ?~ [1, 2, 3]
