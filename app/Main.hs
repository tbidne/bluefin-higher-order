module Main (main) where

import Bluefin.IO (runEff)
import Static qualified
import Dynamic3 qualified

main :: IO ()
main = do
  putStrLn "STATIC"
  runEff $
    \ioe ->
      Static.runEnvironmentIO ioe $ \env ->
        Static.useArgs ioe env ["static", "args"]

  putStrLn "DYNAMIC3 IO"
  runEff $
    \ioe ->
      Dynamic3.runEnvironmentIO ioe $ \env ->
        Dynamic3.useArgs ioe env ["dynamic3 io", "args"]

  putStrLn "DYNAMIC3 STATE"
  runEff $
    \ioe ->
      Dynamic3.runEnvironmentState [] $ \env ->
        Dynamic3.useArgs ioe env ["dynamic3 state", "args"]
