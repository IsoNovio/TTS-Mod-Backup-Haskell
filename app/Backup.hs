module Backup where

import Data.Char
import Data.List
import System.Directory
import System.FilePath
import qualified URLCollector (ModUrlList)
import qualified Network (downloadFile)

urltoFileName :: String -> String
urltoFileName = filter isAlphaNum

guaranteeDir :: FilePath -> IO ()
guaranteeDir = createDirectoryIfMissing False

backup :: FilePath -> FilePath -> Int -> URLCollector.ModUrlList -> IO ()
backup libPath backupPath modNum (bs_, as_, is_, ms_, ps_, ts_) = do
    guaranteeDir backupPath
    guaranteeDir (backupPath ++ "Mods/")
    subfolder "Assetbundles" bs_
    subfolder "Audio" as_
    subfolder "Images" is_
    subfolder "Models" ms_
    subfolder "PDF" ps_
    subfolder "Text" ts_

    guaranteeDir $ backupPath ++ "Mods/Workshop/"
    copyFile (libPath ++ "Mods/Workshop/" ++ show modNum ++ ".png") (backupPath ++ "Mods/Workshop/" ++ show modNum ++ ".png")
    copyFile (libPath ++ "Mods/Workshop/" ++ show modNum ++ ".json") (backupPath ++ "Mods/Workshop/" ++ show modNum ++ ".json")

    where
    subfolder :: String -> [String] -> IO ()
    subfolder subFolderName queryList =
        if queryList == [] then pure ()
        else do
            let libPath_    = libPath    ++ "Mods/" ++ subFolderName ++ "/"
                backupPath_ = backupPath ++ "Mods/" ++ subFolderName ++ "/"
            guaranteeDir backupPath_
            contents <- getDirectoryContents libPath_

            (`mapM_` queryList) $ \queryURL -> do
                let queryPath = urltoFileName queryURL
                case find ((== queryPath) . dropExtension) contents of
                    Just queryPath_ -> copyFile (libPath_ ++ queryPath_) (backupPath_ ++ queryPath_)
                    -- Nothing -> pure ()
                    Nothing -> do
                        extension <- Network.downloadFile queryURL (libPath_ ++ queryPath)
                        if extension /= "" then copyFile (libPath_ ++ queryPath ++ "." ++ extension) (backupPath_ ++ queryPath ++ "." ++ extension)
                        else pure ()