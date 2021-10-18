module Parser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Array ((:))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Show.Generic (genericShow)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

class ParserError (e :: Type) where
  eof :: e
  
  invalidChar :: String -> e

type ParserState a
  = Tuple String a

type ParseFunction e a
  = ParserError e => String -> Either e (ParserState a)

newtype Parser e a
  = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p =
    Parser \s -> (map f) <$> parse p s

instance applyParser :: Apply (Parser e) where
  apply = ap

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance bindParser :: Bind (Parser e) where
  bind p fn =
    Parser \s -> do
      Tuple s' r <- parse p s
      parse (fn r) s'

instance monadParser :: Monad (Parser e)

instance altParser :: Alt (Parser e) where
  alt p1 p2 =
    Parser \s -> case parse p1 s of
      Left _ -> parse p2 s
      Right t -> Right t

instance lazyParser :: Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse (f unit) s

data PError
  = EOF
  | InvalidChar String

derive instance genericPError :: Generic PError _
instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser fn) = fn

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

char :: ∀ e. Parser e Char
char =
  Parser \s ->
    case uncons s of
      Nothing -> Left eof
      Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA =
  Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= \c1 -> char >>= \c2 -> pure $ Tuple c1 c2

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA =
  (\c1 c2 c3 -> fromCharArray [ c1, c2, c3 ])
    <$> char
    <*> char
    <*> char

threeChars :: ∀ e. Parser e String
threeChars = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [ c1, c2, c3 ]

count ::
  ∀ e a f.
  Unfoldable f =>
  Traversable f =>
  Int -> Parser e a -> Parser e (f a)
count n p
  | n <= 0 = pure none
  | otherwise = sequence (replicate n p)

count' ::
  ∀ e.
  Int -> Parser e Char -> Parser e String
count' n p = fromCharArray <$> count n p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional x p = p <|> pure x

atMost :: ∀ e a f. Unfoldable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMost cons n p
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> cons c <$> atMost cons (n - 1) p

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMost (:) n p

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected p =
  char >>= \c -> if p c then pure c else fail (invalidChar expected)

satisfyDo :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfyDo expected p = do
  c <- char
  if p c then
    pure c
  else
    fail (invalidChar expected)

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

range :: ∀ e a f. Semigroup (f a) => Unfoldable f => Traversable f => (a -> f a -> f a) -> Int -> Int -> Parser e a -> Parser e (f a)
range cons min max p
  | min
      > max
      || min
      < 0
      || max
      <= 0 = pure none
  | otherwise = count min p >>= \cs -> (cs <> _) <$> atMost cons (max - min) p

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> range (:) min max p

newtype Year
  = Year Int
derive instance genericYear :: Generic Year _

instance showYear :: Show Year where
  show = genericShow

newtype Month
  = Month Int
derive instance genericMonth :: Generic Month _

instance showMonth :: Show Month where
  show = genericShow

newtype Day
  = Day Int
derive instance genericDay :: Generic Day _

instance showDay :: Show Day where
  show = genericShow

data DateFormat
  = YearFirst
  | MonthFirst
derive instance genericDateFormat :: Generic DateFormat _

instance showDateFormat :: Show DateFormat where
  show = genericShow

type DateParts
  = { year :: Year, month :: Month, day :: Day, format :: DateFormat }

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar = void <<< constChar'

constChar' :: ∀ e. ParserError e => Char -> Parser e Char
constChar' c = satisfy (singleton c) (_ == c)

digitsToNum :: String -> Int
digitsToNum = fromMaybe 0 <<< fromString

yearFirst :: ∀ e. ParserError e => Parser e DateParts
yearFirst = do
  year <- Year <<< digitsToNum <$> count' 4 digit
  constChar '-'
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  day <- Day <<< fromMaybe 0 <<< fromString <$> range' 1 2 digit
  pure $ { year, month, day, format: YearFirst }

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  month <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  day <- Day <<< fromMaybe 0 <<< fromString <$> range' 1 2 digit
  constChar '/'
  year <- Year <<< digitsToNum <$> count' 4 digit
  pure $ { year, month, day, format: MonthFirst }

date :: ∀ e. ParserError e => Parser e DateParts
date = yearFirst <|> monthFirst

some :: ∀ a f m. Unfoldable f => Applicative m => Lazy (m (f a)) => Alt m => (a -> f a -> f a) -> m a -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: ∀ a f m. Unfoldable f => Applicative m => Lazy (m (f a)) => Alt m => (a -> f a -> f a) -> m a -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  a <- range' 1 4 digit
  constChar ','
  constChar ' '
  b <- some' (letter <|> constChar' ' ')
  c <- many' digit
  pure [ a, b, c ]

uglyA :: ∀ e. ParserError e => Parser e (Array String)
uglyA = (\a b c -> [ a, b, c ]) <$> range' 1 4 digit <* constChar ',' <* constChar ' ' <*> some' (letter <|> constChar' ' ') <*> many' digit

uglyB :: ∀ e. ParserError e => Parser e (Array String)
uglyB =
  range' 1 4 digit
    >>= \a ->
        constChar ','
          >>= \_ ->
              constChar ' '
                >>= \_ ->
                    some' (letter <|> constChar' ' ')
                      >>= \b ->
                          many' digit
                            >>= \c -> pure [ a, b, c ]

test :: Effect Unit
test = do
  log $ show $ parse' char "ABC"
  log $ show $ parse' twoCharsA "ABC"
  log $ show $ parse' twoCharsB "ABC"
  log $ show $ parse' twoChars "ABC"
  log $ show $ parse' threeChars "ABC"
  log $ show $ parse' threeChars "A"
  log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"
  log $ show $ parse' (atMost' (-2) alphaNum) "a1b2c3"
  log $ show $ parse' (atMost' 2 alphaNum) "$_$"
  log $ show $ parse' (atMost' 2 alphaNum) "a1b2c3"
  log $ show $ parse' yearFirst "1999-12-31"
  log $ show $ parse' monthFirst "12/31/1999"
  log $ show $ parse' (some' digit) "2343423423abc"
  log $ show $ parse' (many' digit) "_2343423423abc"
  log $ show $ parse' (some' digit) "_2343423423abc"
  log $ show $ parse' ugly "17, some words"
  log $ show $ parse' ugly "5432, some more words1234567890"
