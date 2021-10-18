module Ch27b where

import Prelude
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Control.Parallel (parSequence)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign (Foreign, MultipleErrors)
import Foreign.Generic (decodeJSON, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Class (class Encode, class Decode, defaultOptions)
import Foreign.JSON (parseJSON)

foreign import _reverseKeys :: Foreign -> String

newtype Centimeters
  = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeJsonCentimeters :: Decode Centimeters
instance showCentimeters :: Show Centimeters where
  show = genericShow

newtype Kilograms
  = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeJsonKilograms :: Decode Kilograms
instance showKilograms :: Show Kilograms where
  show = genericShow

newtype Years
  = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeJsonYears :: Decode Years
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
instance decodePersonal :: Decode Personal where
  decode = genericDecode defaultOptions

newtype GPA
  = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeJsonGPA :: Decode GPA
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

instance showGrade :: Show Grade where
  show = genericShow

instance decodeGrade :: Decode Grade where
  decode = genericDecode defaultOptions

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

instance decodeStudent :: Decode Student where
  decode = genericDecode defaultOptions

data TeachingStatus
  = StudentTeacher
  | Probationary
  | NonTenured
  | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode defaultOptions

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

instance decodeTeacher :: Decode Teacher where
  decode = genericDecode defaultOptions

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
              Ajax.post ResponseFormat.string "http://localhost:3000/"
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
            show (processJSON teacherJson :: _ Teacher)
              <> "\n\n"
              <> show (processJSON studentJson :: _ Student)
          Right _ -> "The number of Ajax calls is different than what's being processed."
  where
  processJSON :: ∀ a. Decode a => String -> Either MultipleErrors a
  processJSON json =
    runExcept do
      o <- parseJSON json
      decodeJSON $ _reverseKeys o
