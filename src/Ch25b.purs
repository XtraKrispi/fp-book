module Ch25b where

import Prelude
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parSequence)
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), (.:), (.:?), decodeJson, Json, stringify)
import Data.Argonaut.Decode.Decoders (decodeJObject)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON, genericEncode)
import Foreign.Generic.Class (class Encode, defaultOptions)
import Type.Proxy (Proxy(..))

newtype Centimeters
  = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeJsonCentimeters :: DecodeJson Centimeters
instance showCentimeters :: Show Centimeters where
  show = genericShow

newtype Kilograms
  = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeJsonKilograms :: DecodeJson Kilograms
instance showKilograms :: Show Kilograms where
  show = genericShow

newtype Years
  = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeJsonYears :: DecodeJson Years
instance showYears :: Show Years where
  show = genericShow

newtype Personal
  = Personal
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

derive instance genericPersonal :: Generic Personal _
instance encodePersonal :: Encode Personal where
  encode = genericEncode defaultOptions
instance showPersonal :: Show Personal where
  show = genericShow

instance decodeJsonPersonal :: DecodeJson Personal where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Personal" then do
      contents <- o .: "stnetnoc"
      height <- contents .: "thgieh"
      weight <- contents .: "thgiew"
      age <- contents .: "ega"
      pure
        $ Personal
            { height: height
            , weight: weight
            , age: age
            }
    else
      Left $ AtKey "tag" $ UnexpectedValue json

newtype GPA
  = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeJsonGPA :: DecodeJson GPA
instance showGPA :: Show GPA where
  show = genericShow

data Grade
  = Preschool
  | Kindergarten
  | Grade Int
  | High Int
  | College Int
derive instance genericGrade :: Generic Grade _

instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions

instance decodeJsonGrade :: DecodeJson Grade where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    contents <- o .:? "stnetnoc"
    case Tuple tag contents of
      Tuple "Preschool" Nothing -> pure Preschool
      Tuple "Kindergarten" Nothing -> pure Kindergarten
      Tuple "Grade" (Just num) -> pure $ Grade num
      Tuple "High" (Just num) -> pure $ High num
      Tuple "College" (Just num) -> pure $ College num
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

instance showGrade :: Show Grade where
  show = genericShow

newtype Student
  = Student
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }
derive instance genericStudent :: Generic Student _
instance encodeStudent :: Encode Student where
  encode = genericEncode defaultOptions
instance showStudent :: Show Student where
  show = genericShow

instance decodeJsonStudent :: DecodeJson Student where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Student" then do
      contents <- o .: "stnetnoc"
      grade <- contents .: "edarg"
      teacher <- contents .: "rehcaet"
      gpa <- contents .: "apg"
      personal <- contents .: "lanosrep"
      pure $ Student { grade, teacher, gpa, personal }
    else
      Left $ AtKey "tag" $ UnexpectedValue json

data TeachingStatus
  = StudentTeacher
  | Probationary
  | NonTenured
  | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance decodeJsonTeachingStatus :: DecodeJson TeachingStatus where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    case tag of
      "StudentTeacher" -> pure StudentTeacher
      "Probationary" -> pure Probationary
      "NonTenured" -> pure NonTenured
      "Tenured" -> pure Tenured
      _ -> Left $ AtKey "tag" $ UnexpectedValue json

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

newtype Teacher
  = Teacher
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

derive instance genericTeacher :: Generic Teacher _
instance encodeTeacher :: Encode Teacher where
  encode = genericEncode defaultOptions

instance showTeacher :: Show Teacher where
  show = genericShow

instance decodeJsonTeacher :: DecodeJson Teacher where
  decodeJson json = do
    o <- decodeJObject json
    tag <- o .: "gat"
    if tag == "Teacher" then do
      contents <- o .: "stnetnoc"
      grades <- contents .: "sedarg"
      numberOfStudents <- contents .: "stnedutSfOrebmun"
      personal <- contents .: "lanosrep"
      status <- contents .: "sutats"
      pure $ Teacher { grades, numberOfStudents, personal, status }
    else
      Left $ AtKey "tag" $ UnexpectedValue json

testTeacher :: Teacher
testTeacher =
  Teacher
    { grades: [ Preschool, Kindergarten, Grade 1 ]
    , numberOfStudents: 23
    , personal:
        Personal
          { height: Centimeters 162.56
          , weight: Kilograms 63.5
          , age: Years 31
          }
    , status: NonTenured
    }

testStudent :: Student
testStudent =
  Student
    { grade: Grade 1
    , teacher: testTeacher
    , gpa: GPA 3.2
    , personal:
        Personal
          { height: Centimeters 107.9
          , weight: Kilograms 17.9
          , age: Years 5
          }
    }

processAjaxResult ::
  ∀ a.
  Show a =>
  DecodeJson a =>
  Proxy a ->
  Either Ajax.Error (Ajax.Response Json) ->
  String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } -> do
    case (decodeJson body :: _ a) of
      Left err -> show err
      Right reversedRecord -> show reversedRecord

-- test :: Effect Unit
-- test =
--   launchAff_ do
--     result <-
--       Ajax.post ResponseFormat.json "http://localhost:3000/"
--         $ Just
--         $ RequestBody.String
--         $ encodeJSON testTeacher
--     log $ show $ bimap Ajax.printError (stringify <<< _.body) $ result
--     log $ processAjaxResult (Proxy :: _ Teacher) result
test :: Effect Unit
test =
  launchAff_ do
    results <-
      parSequence
        $ ( \json ->
              Ajax.post ResponseFormat.json "http://localhost:3000/"
                $ Just
                $ RequestBody.String json
          )
        <$>
          [ encodeJSON testTeacher
          , encodeJSON testStudent
          ]
    log
      $ case map _.body <$> sequence results of
          Left err -> Ajax.printError err
          Right [ teacherJson, studentJson ] ->
            show (decodeJson teacherJson :: _ Teacher)
              <> "\n\n"
              <> show (decodeJson studentJson :: _ Student)
          Right _ -> "The number of Ajax calls is different than what's being processed."
