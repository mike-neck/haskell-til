module Main where

import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200, hContentType)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (pack)
import Data.List (lookup)
import Data.Maybe

main :: IO ()
main = do
  port <- getPort
  putStrLn $ "start Server: http://localhost:" ++ (show port)
  run port helloApp

helloApp :: Application
helloApp req res = res $ responseLBS status200 [(hContentType, pack "text/plain")] helloWai

helloWai :: LB.ByteString
helloWai = toLazyByteString $ stringUtf8 "Hello Wai"

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort ::  Port
defaultPort = 3000
