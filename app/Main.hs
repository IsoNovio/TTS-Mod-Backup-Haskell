{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
import Data.Char
import Data.Maybe
import Data.Functor
import Data.Traversable
import Text.JSON
import qualified PathHelper as Path
import qualified URLCollector as URL
import qualified NameCollector as Name
import qualified Backup (backup)

main :: IO ()
main = do
    libPath <- queryLibPath
    backupPath <- queryBackupPath
    putStr "\n\n"

    modItems <- listModItems libPath
    putStr "\n\n"

    choose1 libPath backupPath modItems

choose1 :: FilePath -> FilePath -> [(Int, (ModItem, JSValue))] -> IO ()
choose1 libPath backupPath modItems = do
    (backupModNum, backupTgtPath, backupJSON) <- getBackupTgt modItems    
    modURLs <- getModURLs backupJSON

    let backupPath_ = backupPath ++ backupTgtPath ++ "/"
    Backup.backup libPath backupPath_ backupModNum modURLs
    
    putStrLn $ "You have backed up " ++ backupTgtPath
    putStrLn $ "Do you want to backup another mod? (y/n)"
    f <- getLine
    case f of
        "y" -> choose1 libPath backupPath modItems
        _ -> pure ()

---- query directories ----

queryLibPath :: IO FilePath
queryLibPath = do
    putStrLn "Where is you TTS library?"
    putStrLn "Empty means the default location."
    putStrLn "The directory must be valid."
    dir <- getLine
    if dir /= "" then
        if last dir == '/' then pure dir
        else pure $ dir ++ "/"
    else Path.getDefaultLibPath

queryBackupPath :: IO FilePath
queryBackupPath = do
    putStrLn "Where do you want to backup this mod?"
    putStrLn "Empty means the Downloads folder."
    putStrLn "The directory must be valid."
    dir <- getLine
    if dir /= "" then
        if last dir == '/' then pure dir
        else pure $ dir ++ "/"
    else Path.getDefaultBackupPath


---- list URLs ----

getModURLs :: JSValue -> IO URL.ModUrlList
getModURLs jsValue = do
    let modURLs = URL.findURLs jsValue
    return modURLs

getBackupTgt :: [(Int, (ModItem, JSValue))] -> IO (Int, String, JSValue)
getBackupTgt modItems = do
    putStrLn "Which one do you want to backup? (Please specity the index)"
    ix_ <- getLine
    if all isDigit ix_ then
        let ix = read ix_
        in case ix `lookup` modItems of
            Just result@(item@(ModItem _ _ num), jsValue) -> do
                putStr "You have optioned to back up: "
                printModItem (ix, result)
                return (num, modFolderName item, jsValue)
            Nothing ->
                let lastIx = length modItems
                in do
                    putStrLn $ "Please refer a valid integer between 1 and " ++ show lastIx ++ "."
                    getBackupTgt modItems
    else do
        putStrLn "Please refer a number."
        getBackupTgt modItems


---- List Mod Items ----

data ModItem = ModItem {modPath :: FilePath, modName :: String, modNum :: Int}

listModItems :: FilePath -> IO [(Int, (ModItem, JSValue))]
listModItems path = do
    modItems <- getModItems path
    putStrLn "You have these mods in your TTS library:"
    _ <- modItems `for` printModItem
    return modItems

getModItems :: FilePath -> IO [(Int, (ModItem, JSValue))]
getModItems path =
    let path_ = path ++ "Mods/Workshop/"
    in getDirectoryContents path_ <&> map (path_ ++) >>= traverse getModItem <&> catMaybes <&> label 1

getModItem :: FilePath -> IO (Maybe (ModItem, JSValue))
getModItem path =
    if Path.isJSON path then do
        json <- readFile path
        pure $ case decodeStrict json of
                    Error _ -> Nothing
                    Ok jsValue ->
                        case (Name.getSaveName jsValue, Path.getModNum path) of
                            (Just name, Just num) -> Just (ModItem path name num, jsValue)
                            _ -> Nothing
    else pure Nothing

label :: Int -> [a] -> [(Int, a)]
label _ix [] = []
label ix (a:as) = (ix,a) : label (ix+1) as

modFolderName :: ModItem -> String
modFolderName (ModItem _ name num) = name ++ " (" ++ show num ++ ")"

showModItem :: (Int, (ModItem, JSValue)) -> String
showModItem (ix, (modItem, _)) = "[" ++ show ix ++ "] " ++ modFolderName modItem

printModItem :: (Int, (ModItem, JSValue)) -> IO ()
printModItem = putStrLn . showModItem