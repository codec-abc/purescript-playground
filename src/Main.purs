module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import PolishNotationEvaluator (evaluate)

someString :: String
someString = "- 5 * 6 7"

someString2 :: String
someString2 = "- * / 15 - 7 + 1 1 3 + 2 + 1 1"

someString3 :: String
someString3 = "* / 15 - 7 + 1 1 3 + 2 + 1 1"

someString4 :: String
someString4 = "* / 15 - 7 + 1 1 3 + 2 + 1"

someString5 :: String
someString5 = "* 15 - 7 + 1 1 3 + 2 + 1 1"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
    log ( show ( evaluate someString))
    log ( show ( evaluate someString2))
    log ( show ( evaluate someString3))
    log ( show ( evaluate someString4))
    log ( show ( evaluate someString5))
