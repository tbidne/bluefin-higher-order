module Main (main) where

import Bluefin.IO (runEff)
import Static qualified

main :: IO ()
main = do
  putStrLn "STATIC"
  runEff $
    \ioe ->
      Static.runEnvironmentIO ioe $ \env ->
        Static.useArgs ioe env ["static", "args"]
