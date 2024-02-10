module PathHelper where

import System.Environment
import System.FilePath
import Data.Char

getDefaultLibPath :: IO FilePath
getDefaultLibPath = do
    user <- getEnv "USER"
    pure $ "/Users/" ++ user ++ "/Library/Tabletop Simulator/"

isJSON :: FilePath -> Bool
isJSON = isExtensionOf "json"

getModNum :: FilePath -> Maybe Int
getModNum = guard_ . dropExtension . takeFileName where
    guard_ str = if all isDigit str then Just $ read str else Nothing

getDefaultBackupPath :: IO FilePath
getDefaultBackupPath = do
    user <- getEnv "USER"
    pure $ "/Users/" ++ user ++ "/Downloads/"
