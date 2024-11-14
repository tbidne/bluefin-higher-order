module Dynamic1
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
import Data.Kind (Type)
import System.Environment qualified as Env
import UnliftIO (MonadUnliftIO (withRunInIO))
import Utils qualified
import Bluefin.Compound (useImpl, useImplIn)

type Environment :: Effects -> Type
data Environment es = MkEnvironment
  { getArgsImpl :: Eff es [String],
    withArgsImpl :: forall a. [String] -> Eff es a -> Eff es a
  }

getArgs :: forall e es. (e :> es) => Environment e -> Eff es [String]
getArgs e = useImpl @e @es (getArgsImpl e)

withArgs ::
  forall es a.
  Environment es ->
  [String] ->
  Eff es a ->
  Eff es a
withArgs e xs eff = useImpl (withArgsImpl e xs eff)

runEnvironmentIO ::
  forall envEff es r.
  (envEff :> es) =>
  IOE envEff ->
  (forall e. Environment e -> Eff (e :& es) r) ->
  Eff es r
runEnvironmentIO ioe k =
  useImplIn
    k
    MkEnvironment
      { getArgsImpl = effIO ioe Env.getArgs,
        withArgsImpl = \xs eff ->
          runEffReader ioe $
            withRunInIO $
              \runInIO -> Env.withArgs xs (runInIO . toReader $ eff)
      }

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

  -- - Couldn't match type ‘e2’ with ‘es’
  --   Expected: Environment es
  --     Actual: Environment e2
  --withArgs env args $ do
  --  argsIn <- getArgs env
  --  Utils.putStrLn ioe ("withArgs: " ++ show argsIn)

  argsAfter <- getArgs env
  Utils.putStrLn ioe ("after: " ++ show argsAfter)
