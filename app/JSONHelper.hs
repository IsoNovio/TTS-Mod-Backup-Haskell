module JSONHelper where

import Data.Bifunctor
import Text.JSON

simpleEval ::
        a
    -> (Bool -> a)
    -> (Bool -> Rational -> a)
    -> (String -> a)
    -> ([a] -> a)
    -> ([(String, a)] -> a) --Either ([(String, JSValue)] -> JSValue) 
    -> JSValue
    -> a
simpleEval fNull fBool fRational fString fArray fObject = eval where
    eval JSNull = fNull
    eval (JSBool b) = fBool b
    eval (JSRational b r) = fRational b r
    eval (JSString jsStr) = fString $ fromJSString jsStr
    eval (JSArray vals) = fArray $ eval `map` vals
    eval (JSObject objs) = fObject $ second eval `map` fromJSObject objs
        --     case fObject of
        -- Left fl -> eval . fl $ fromJSObject objs                -- filter then eval
        -- Right fr -> fr $ second eval `map` fromJSObject objs    -- eval then filter

const2 :: a -> b1 -> b2 -> a
const2 = const . const