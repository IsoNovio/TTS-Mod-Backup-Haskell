module URLCollector where

import Data.Char
import qualified Data.Set as Set
import Control.Monad.State
import Text.JSON
import JSONDecoder as JS


---- ModURLs ----

data ModURLs = ModURLs {
    assetbundles :: Set.Set String,
    audio :: Set.Set String,
    images :: Set.Set String,
    models :: Set.Set String,
    pdf :: Set.Set String,
    text :: Set.Set String
    } deriving Show
instance Semigroup ModURLs where
    ModURLs b1 a1 i1 m1 p1 t1 <> ModURLs b2 a2 i2 m2 p2 t2 =
        ModURLs (b1 `Set.union` b2) (a1 `Set.union` a2) (i1 `Set.union` i2) (m1 `Set.union` m2) (p1 `Set.union` p2) (t1 `Set.union` t2)
instance Monoid ModURLs where
    mempty = ModURLs Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty

type ModUrlList = ([String], [String], [String], [String], [String], [String])

toList :: ModURLs -> ModUrlList
toList (ModURLs bs as is ms ps ts) = (Set.toList bs, Set.toList as, Set.toList is, Set.toList ms, Set.toList ps, Set.toList ts)


---- URLState and getting URLs ----

type URLState = State ModURLs

addAssetbundles, addAudio, addImages, addModels, addPDF, addText :: String -> URLState ()
addAssetbundles url = modify $ \modURLs -> modURLs {assetbundles    = Set.insert url $ assetbundles modURLs}
addAudio        url = modify $ \modURLs -> modURLs {audio           = Set.insert url $ audio        modURLs}
addImages       url = modify $ \modURLs -> modURLs {images          = Set.insert url $ images       modURLs}
addModels       url = modify $ \modURLs -> modURLs {models          = Set.insert url $ models       modURLs}
addPDF          url = modify $ \modURLs -> modURLs {pdf             = Set.insert url $ pdf          modURLs}
addText         url = modify $ \modURLs -> modURLs {text            = Set.insert url $ text         modURLs}

findURLs :: JSValue -> ModUrlList
findURLs jsValue = toList $ eval jsValue `execState` mempty where
    
    eval = JS.simpleEval
        justEmpty
        (const justEmpty)
        (const2 justEmpty)
        pure
        (fmap concat . sequence)
        ((>> justEmpty) . traverse addURL)
    
    justEmpty = pure ""
    isURL = (== "http://") . take 7
    
    addURL (str, a_) = do
        a <- a_
        if isURL a then
            if str `elem` ["TableURL", "SkyURL", "ImageURL", "ImageSecondaryURL", "FaceURL", "BackURL", "DiffuseURL"]
                then addImages a
            else if str `elem` ["URL", "AssetbundleURL", "AssetbundleSecondaryURL"]
                then addAssetbundles a
            else if str `elem` ["MeshURL", "ColliderURL"]
                then addModels a
            else if str == "PDFUrl"
                then addPDF a
            -- no custom text here
            else if take 4 str == "Item" && all isDigit (drop 4 str)
                then addAudio a
            else pure ()
        else pure ()