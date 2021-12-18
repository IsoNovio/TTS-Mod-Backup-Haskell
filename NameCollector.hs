module NameCollector where

import Control.Monad
import Text.JSON
import qualified JSONDecoder as JS

getSaveName :: JSValue -> Maybe String
getSaveName = JS.simpleEval
    Nothing
    (const Nothing)
    (JS.const2 Nothing)
    Just
    (const Nothing)
    (join . lookup "SaveName")