module Utils
  ( print,
    putStrLn,
  )
where

import Bluefin.Eff (Eff, type (:>))
import Bluefin.IO (IOE, effIO)
import Prelude (Show, String, (.))
import Prelude qualified as P

print :: (e :> es, Show a) => IOE e -> a -> Eff es ()
print ioe = effIO ioe . P.print

putStrLn :: (e :> es) => IOE e -> String -> Eff es ()
putStrLn ioe = effIO ioe . P.putStrLn
