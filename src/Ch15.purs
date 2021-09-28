module Ch15 where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a
  = Predicate (a -> Boolean)

instance contravariantPredicate :: Contravariant Predicate where
  cmap fba (Predicate p) = Predicate $ p <<< fba

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate p) = p

data Moore s a b
  = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap ca bd (Moore s sb sas) =
    Moore s (bd <<< sb) (\s0 -> sas s0 <<< ca)

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ s a b f. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s output transition) = output <<< foldl transition s

sizer :: Moore Int String String
sizer = dimap length (\l -> "Size is " <> show l) addr

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log "------------------------------------"
  log $ show $ runPredicate (cmap (_ + 1) (Predicate odd)) 10
  log $ show $ runPredicate (cmap (_ + 2) (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [ 1, 2, 3 ]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]
