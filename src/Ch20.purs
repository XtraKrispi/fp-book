module Ch20 where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Control.Monad.State.Trans (class MonadState, StateT, state, get, put, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Trans (class MonadTell, WriterT, runWriterT, tell)
import Control.Monad.Reader.Trans (class MonadAsk)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

-- newtype WriterT w m a
--   = WriterT (m (Tuple a w))
-- runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
-- runWriterT (WriterT mx) = mx
-- instance functorWriterT :: Functor m => Functor (WriterT w m) where
--   map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w
-- instance applyWriterT :: (Semigroup w, Monad m) => Apply (WriterT w m) where
--   apply (WriterT mf) (WriterT mx) =
--     WriterT do
--       Tuple f w1 <- mf
--       Tuple x w2 <- mx
--       pure $ Tuple (f x) (w1 <> w2)
-- instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
--   pure x = WriterT $ pure $ Tuple x mempty
-- instance bindWriterT :: (Monoid w, Monad m) => Bind (WriterT w m) where
--   bind :: ∀ a b. WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
--   bind (WriterT mx) f =
--     WriterT do
--       Tuple x w1 <- mx
--       Tuple y w2 <- runWriterT $ f x
--       pure $ Tuple y $ w1 <> w2
-- instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)
newtype ReaderT :: forall k. Type -> (k -> Type) -> k -> Type
newtype ReaderT r m a
  = ReaderT (r -> m a)

runReaderT :: ∀ r m a. ReaderT r m a -> r -> m a
runReaderT (ReaderT mf) = mf

instance functorReaderT :: Functor m => Functor (ReaderT r m) where
  map f (ReaderT mg) =
    ReaderT \r -> f <$> mg r

instance applyReaderT :: Apply m => Apply (ReaderT r m) where
  apply (ReaderT fmf) (ReaderT fmx) = ReaderT \r -> fmf r <*> fmx r

instance applicativeReaderT :: Monad m => Applicative (ReaderT r m) where
  pure = lift <<< pure

instance bindReaderT :: Bind m => Bind (ReaderT r m) where
  bind (ReaderT fmx) f =
    ReaderT \r -> fmx r >>= \x -> runReaderT (f x) r

instance monadReaderT :: Monad m => Monad (ReaderT r m)

instance monadTransReaderT :: MonadTrans (ReaderT r) where
  lift = ReaderT <<< const

instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
  ask = ReaderT pure

instance monadTellReaderT :: MonadTell w m => MonadTell w (ReaderT r m) where
  tell = lift <<< tell

instance monadStateReaderT :: MonadState s m => MonadState s (ReaderT r m) where
  state = lift <<< state

type AppStack e w s a
  = ExceptT e (WriterT w (StateT s Effect)) a

type AppM
  = AppStack String String Int Unit

type StackResult
  = Tuple (Tuple (Either String Unit) String) Int

type AppResult
  = Tuple (Maybe String) AppEffects

type AppEffects
  = { log :: String
    , state :: Int
    , result :: Maybe Unit
    }

runApp ::
  Int ->
  AppM ->
  Effect AppResult
runApp s = map results <<< flip runStateT s <<< runWriterT <<< runExceptT

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right _) l) s) = Tuple Nothing { log: l, state: s, result: Just unit }

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  log "Starting App..."
  n <- get
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"
  put $ n + 1
  log "Incremented State"
  pure unit

test :: Effect Unit
test = do
  result1 <- runApp 0 app
  Console.log $ show result1
  result2 <- runApp 99 app
  Console.log $ show result2
