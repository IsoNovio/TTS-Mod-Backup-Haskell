module Network where

import Prelude
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.Char (isAlphaNum)
import Data.ByteString.Lazy as L
-- import Control.Exception as X

contentTypeToExtension :: String -> String
contentTypeToExtension = Prelude.takeWhile isAlphaNum . Prelude.tail . Prelude.dropWhile (/= '/')

downloadFile :: String -> FilePath -> IO String
downloadFile url filePath_ = do
    request <- parseRequest url
    response <- httpLBS request -- `X.catch` statusExceptionHandler
    let statusCode = show $ getResponseStatusCode response
    if statusCode == "200" then do
        let contentType = show $ Prelude.head $ getResponseHeader hContentType response
        let extension = contentTypeToExtension contentType
        let filePath = filePath_ ++ "." ++ extension
        L.writeFile filePath $ getResponseBody response
        putStrLn $ "File downloaded to " ++ filePath
        pure extension
    else do
        putStrLn $ "Failed to retrieve " ++ url
        pure ""

-- main :: IO ()
-- main = do
--     let url = "http://cloud-3.steamusercontent.com/ugc/1840280459060410754/436C88E26D345A91A027265C2EC135E66C67A0E6/"
--     let filePath = "/Users/wangyinong/Downloads/1"
--     x <- downloadFile url filePath
--     putStrLn $ show x
--     pure ()