module Ch11 where

import Data.Foldable (class Foldable)
import Data.List (List(..), foldMap, foldl, foldr, singleton, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (<<<), (<>), (>))

reverse :: List ~> List
reverse = foldl (\xs x -> x : xs) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y
  | y > x = y
  | otherwise = x

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ a f. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 fn (x :| xs) = foldl fn x xs

sum :: ∀ f a. Foldable f => Semiring a => f a -> a
sum = foldl (+) zero

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf a) = singleton a
toList (Node l r) = toList l <> toList r

instance foldableTree :: Foldable Tree where
  foldr fn acc = foldr fn acc <<< toList
  foldl fn acc = foldl fn acc <<< toList
  foldMap fn = foldMap fn <<< toList

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax (37 : 311 : (-1) : 2 : 84 : Nil)
  log $ show $ findMax ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxNE (NonEmptyList $ 37 :| (311 : -1 : 2 : 84 : Nil))
  log $ show $ findMaxNE (NonEmptyList $ "a" :| ("bbb" : "c" : Nil))
  log $ show $ sum (1 : 2 : 3 : Nil)
  log $ show $ sum (1.0 : 2.0 : 3.0 : Nil)
  log $ show $ sum [ 1, 2, 3 ]
  log $ show $ sum [ 1.0, 2.0, 3.0 ]
  log
    $ show
    $ toList
        (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log
    $ show
    $ sum
        (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
