module Ch23b where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State.Trans (StateT, get, modify_, runStateT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, launchAff_)
import Effect.Aff.Bus (BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

type Config = { bus :: BusRW String }
type State = { count :: Int }

type FiberM a = ReaderT Config (StateT State Aff) a

runFiberM :: BusRW String -> FiberM Unit  -> Aff Unit
runFiberM bus = void <<< forkAff <<< flip runStateT {count: 10} <<< flip runReaderT { bus }

liftAffToFiberM :: Aff ~> FiberM
liftAffToFiberM = lift <<< lift

randomAff :: Aff Number
randomAff = liftEffect random

delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> randomAff

logger :: FiberM Unit
logger = forever do
  {bus} <- ask
  s <- liftAffToFiberM $ Bus.read bus
  log $ "Logger: " <> s

randomGenerator :: String -> (Number -> Boolean) -> FiberM Unit
randomGenerator valueType pred = do
  { count } <- lift $ get
  unless (count <= 0) do
    { bus } <- ask
    liftAffToFiberM do
      n <- delayRandom
      when (pred n) $ flip Bus.write bus $ "Found a value that is " <> valueType <> "(" <> show n <> ")"
    modify_ _{count = count - 1}
    randomGenerator valueType pred
   

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM logger
  forkFiberM $ randomGenerator "greater than 0.5" (_ > 0.5)
  forkFiberM $ randomGenerator "less than 0.5" (_ < 0.5)
  forkFiberM $ randomGenerator "greater than 0.1" (_ > 0.1)