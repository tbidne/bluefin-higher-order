{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Reader.Dynamic
  ( Reader (..),
  )
where

import Bluefin.Eff (Eff, (:&), type (:>), Effects)
import Bluefin.IO (IOE, runEff)
import Data.List qualified as L
import Utils qualified
import Prelude hiding (log)
import Data.Kind (Type)
import Bluefin.Compound (useImpl, useImplIn)

-- FIXME: -- Implementing reader

type Reader :: Type -> Effects -> Type
data Reader r es = MkReader
  { askImpl :: Eff es r,
    localImpl ::
      forall e a.
        (r -> r) ->
        (Reader r e -> Eff es a) ->
        Eff (e :& es) a
  }

ask :: forall r e es. (e :> es) => Reader r e -> Eff es r
ask e = useImpl (askImpl e)

local ::
  forall r e es a.
  ( e :> es
  ) =>
  Reader r e ->
  (r -> r) ->
  (forall e1. Reader r e1 -> Eff es a) ->
  Eff es a
-- • Couldn't match type ‘es’ with ‘e’ --------------------
--   Expected: Reader r e0 -> Eff e a                     |
--     Actual: Reader r e0 -> Eff es a                    v
local e f onRdr = error "todo" --useImpl (localImpl e f onRdr)

runReader ::
  forall r es.
  r ->
  (forall e. Reader r e -> Eff (e :& es) r) ->
  Eff es r
runReader env k =
  useImplIn
    k
    MkReader
      { askImpl = pure env,
        localImpl = \modEnv onRdr -> error "todo"
      }

{-

-- λ. run
-- [foo]: something
-- [foo.doThing]: more logs
run :: IO ()
run = runEff $ \ioe -> [] $ \rdr -> foo ioe rdr

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
-}
