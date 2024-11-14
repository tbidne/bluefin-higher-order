{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Static
  ( -- * Effect
    Environment (..),
    getArgs,
    withArgs,

    -- * Interpreter
    runEnvironmentIO,

    -- * Usage
    useArgs,
  )
where

import Bluefin.Eff (Eff, Effects, (:&), type (:>))
import Bluefin.IO
  ( EffReader,
    IOE,
    effIO,
    effReader,
    runEffReader,
  )
import Bluefin.Internal (unsafeRemoveEff)
import Data.Kind (Type)
import System.Environment qualified as Env
import UnliftIO (MonadUnliftIO (withRunInIO))
import Utils qualified

type Environment :: Effects -> Type
newtype Environment e = MkEnvironment (IOE e)

getArgs :: (e :> es) => Environment e -> Eff es [String]
getArgs (MkEnvironment ioe) = effIO ioe Env.getArgs

withArgs ::
  (e :> es) =>
  Environment e ->
  [String] ->
  Eff es a ->
  Eff es a
withArgs (MkEnvironment ioe) xs eff =
  runEffReader ioe $
    withRunInIO $
      \runInIO -> Env.withArgs xs (runInIO . toReader $ eff)

runEnvironmentIO ::
  forall envEff es r.
  (envEff :> es) =>
  IOE envEff ->
  (forall e. Environment e -> Eff (e :& es) r) ->
  Eff es r
runEnvironmentIO ioe k = unsafeRemoveEff (k $ MkEnvironment ioe)

toReader :: Eff es a -> EffReader r es a
toReader = effReader . const

useArgs ::
  ( e1 :> es,
    e2 :> es
  ) =>
  IOE e1 ->
  Environment e2 ->
  [String] ->
  Eff es ()
useArgs ioe env args = do
  argsBefore <- getArgs env
  Utils.putStrLn ioe ("before: " ++ show argsBefore)

  withArgs env args $ do
    argsIn <- getArgs env
    Utils.putStrLn ioe ("withArgs: " ++ show argsIn)

  argsAfter <- getArgs env
  Utils.putStrLn ioe ("after: " ++ show argsAfter)
