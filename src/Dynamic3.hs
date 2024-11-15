module Dynamic3
  ( -- * Effect
    Environment (..),
    getArgs,
    withArgs,

    -- * Interpreter
    runEnvironmentIO,
    runEnvironmentState,

    -- * Usage
    useArgs,
  )
where

import Bluefin.Compound (Handle, mapHandle, useImpl, useImplIn)
import Bluefin.Eff (Eff, Effects, bracket, (:&), type (:>))
import Bluefin.IO
  ( IOE,
    effIO,
    withEffToIO,
  )
import Bluefin.Internal (inContext, insertManySecond)
import Bluefin.State (evalState, get, put)
import Data.Kind (Type)
import System.Environment qualified as Env
import Utils qualified

type Environment :: Effects -> Type
data Environment es = MkEnvironment
  { getArgsImpl :: forall e. Eff (e :& es) [String],
    withArgsImpl :: forall a e. [String] -> Eff e a -> Eff (e :& es) a
  }

-- This is not necessary for the code in this module, but I think it's
-- nice to see.
instance Handle Environment where
  mapHandle e =
    MkEnvironment
      { getArgsImpl = insertManySecond (getArgsImpl e),
        withArgsImpl =
          \xs eff -> insertManySecond (withArgsImpl e xs eff)
      }

getArgs :: forall e es. (e :> es) => Environment e -> Eff es [String]
getArgs e = inContext (getArgsImpl e)

withArgs ::
  forall e es a.
  (e :> es) =>
  Environment e ->
  [String] ->
  Eff es a ->
  Eff es a
withArgs e xs eff = inContext (withArgsImpl e xs eff)

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
          withEffToIO
            (\effToIO -> Env.withArgs xs (effToIO (\_ -> useImpl eff)))
            ioe
      }

runEnvironmentState ::
  forall es r.
  [String] ->
  (forall e. Environment e -> Eff (e :& es) r) ->
  Eff es r
runEnvironmentState initial k =
  evalState initial $ \args ->
    useImplIn
      k
      MkEnvironment
        { getArgsImpl = get args,
          withArgsImpl = \xs eff -> do
            orig <- get args
            bracket
              (put args xs)
              (\() -> put args orig)
              (\() -> useImpl eff)
        }

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
