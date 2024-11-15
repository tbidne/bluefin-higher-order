{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Reader.Static
  ( run
  )
where

import Bluefin.Eff (Eff, (:&), type (:>))
import Bluefin.IO (IOE, runEff)
import Bluefin.Internal (Reader (MkReader))
import Bluefin.Reader (ask, runReader)
import Data.List qualified as L
import Utils qualified
import Prelude hiding (log)

-- Implementing static:
--
--     local :: MonadReader r m => (r -> r) -> m a -> m a

-- λ. run
-- [foo]: something
-- [foo.doThing]: more logs
run :: IO ()
run = runEff $ \ioe -> runReader [] $ \rdr -> foo ioe rdr

local ::
  forall r e es a.
  (e :> es) =>
  Reader r e ->
  (r -> r) ->
  (forall e1. Reader r e1 -> Eff (e1 :& es) a) ->
  Eff es a
local (MkReader ns) f = runReader (f ns)

addNamespace ::
  forall e es a.
  (e :> es) =>
  Reader [String] e ->
  String ->
  (forall e1. Reader [String] e1 -> Eff (e1 :& es) a) ->
  Eff es a
addNamespace r ns = local r (++ [ns])

foo ::
  forall e1 e2 es.
  ( e1 :> es,
    e2 :> es
  ) =>
  IOE e1 ->
  Reader [String] e2 ->
  Eff es ()
foo io rdr = addNamespace rdr "foo" $ \rdr2 -> do
  log io rdr2 "something"
  addNamespace rdr2 "doThing" $ \rdr3 -> log io rdr3 "more logs"

log ::
  forall e1 e2 es.
  ( e1 :> es,
    e2 :> es
  ) =>
  IOE e1 ->
  Reader [String] e2 ->
  String ->
  Eff es ()
log io rdr s = do
  env <- ask rdr
  Utils.putStrLn io $ fmt env s
  where
    fmt ns m =
      mconcat
        [ "[",
          L.intercalate "." ns,
          "]: ",
          m
        ]
