module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (<<<), (==), (>), (>=), max)
import Undefined (undefined)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip fn b a = fn a b

const :: ∀ a b. a -> b -> a
const a _ = a

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixr 0 apply as $

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton a = a : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: ∀ a. List a -> Int
length = go 0
  where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : ys) = go (acc + 1) ys

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l
  where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (x : _) 0 = Just x
index (_ : xs) n
  | n < 0 = Nothing
  | otherwise = index xs (n - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex p = go 0
  where
  go _ Nil = Nothing
  go acc (x : xs)
    | p x = Just acc
    | otherwise = go (acc + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex p = go Nothing 0
  where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go acc _ Nil = acc
  go acc i (x : xs) =
    go (if p x then Just i else acc) (i + 1) xs

reverse :: List ~> List
reverse = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter p = reverse <<< go Nil
  where
  go acc Nil = acc
  go acc (x : xs) =
    if p x then
      go (x : acc) xs
    else
      go acc xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes = reverse <<< go Nil
  where
  go acc Nil = acc
  go acc (Nothing : xs) = go acc xs
  go acc (Just x : xs) = go (x : acc) xs

range :: Int -> Int -> List Int
range start end = go Nil end
  where
  step = if start < end then (-1) else 1
  go acc e
    | e == start = e : acc
    | otherwise = go (e : acc) (e + step)

take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go (max 0 n) Nil
  where
  go _ acc Nil = acc
  go 0 acc _ = acc
  go n' acc (x : xs) = go (n' - 1) (x : acc) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 xs = xs
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile p (x : xs)
  | p x = x : takeWhile p xs
  | otherwise = Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile p l@(x : xs)
  | p x = dropWhile p xs
  | otherwise = l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = snd <<< go
  where
  go :: List a -> Tuple Int (List a)
  go Nil = Tuple 0 Nil
  go (x : xs) =
    let
      Tuple i ys = go xs
    in
      Tuple (i + 1) (if i < n then x : ys else ys)

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = snd <<< go
  where
  go Nil = Tuple 0 Nil
  go (x : xs) =
    let
      Tuple i ys = go xs
    in
      Tuple (i + 1) (if i < n then ys else x : ys)

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple a b : xs) =
  let
    Tuple as bs = unzip xs
  in
    Tuple (a : as) (b : bs)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null $ singleton "abc"
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil
  log $ show $ head (Nil :: List Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $ tail (Nil :: List Unit)
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ last (Nil :: List Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log $ show $ init (Nil :: List Unit)
  log $ show $ init $ 1 : Nil
  log $ show $ init $ 1 : 2 : Nil
  log $ show $ init $ 1 : 2 : 3 : Nil
  log $ show $ uncons $ 1 : 2 : 3 : Nil
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log
    $ show
    $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log
    $ show
    $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 ((-7) : 9 : 0 : 12 : (-13) : 45 : 976 : (-19) : Nil)
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
  log $ show $ dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ dropEnd 10 (1 : Nil)
  log $ show $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ zip (Nil :: List Unit) (1 : 2 : Nil)
  log $ show $ unzip (Tuple 1 "a" : Tuple 2 "b" : Tuple 3 "c" : Nil)
  log $ show $ unzip (Tuple "a" 1 : Tuple "b" 2 : Tuple "c" 3 : Nil)
  log $ show $ unzip (Nil :: List (Tuple Unit Unit))
