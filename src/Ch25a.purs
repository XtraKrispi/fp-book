module Ch25a where

import Prelude
import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Foreign.Generic (SumEncoding(..), decodeJSON, encodeJSON, genericDecode, genericEncode)
import Foreign.Generic.Class (class Decode, class Encode, Options, defaultOptions)
import Type.Proxy (Proxy(..))

newtype Centimeters
  = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance encodeCentimeters :: Encode Centimeters
derive newtype instance decodeCentimeters :: Decode Centimeters
instance showCentimeters :: Show Centimeters where
  show = genericShow

-- instance encodeCentimeters :: Encode Centimeters where
--   encode = genericEncode defaultOptions
-- instance decodeCentimeters :: Decode Centimeters where
--   decode = genericDecode defaultOptions
newtype Kilograms
  = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance encodeKilograms :: Encode Kilograms
derive newtype instance decodeKilograms :: Decode Kilograms
instance showKilograms :: Show Kilograms where
  show = genericShow

newtype Years
  = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance encodeYears :: Encode Years
derive newtype instance decodeYears :: Decode Years
instance showYears :: Show Years where
  show = genericShow

type Personal
  = { height :: Centimeters
    , weight :: Kilograms
    , age :: Years
    }

newtype GPA
  = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance encodeGPA :: Encode GPA
derive newtype instance decodeGPA :: Decode GPA
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

instance decodeGrade :: Decode Grade where
  decode = genericDecode decodeOptions

instance showGrade :: Show Grade where
  show = genericShow

type Student
  = { grade :: Grade
    , teacher :: Teacher
    , gpa :: GPA
    , personal :: Personal
    }

data TeachingStatus
  = Student
  | Probationary
  | NonTenured
  | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _

instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions

instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode decodeOptions

instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

type Teacher
  = { grades :: Array Grade
    , numberOfStudents :: Int
    , personal :: Personal
    , status :: TeachingStatus
    }

type ReversedPersonal
  = { thgieh :: Centimeters
    , thgiew :: Kilograms
    , ega :: Years
    }

type ReversedStudent
  = { edarg :: Grade
    , rehcaet :: ReversedTeacher
    , apg :: GPA
    , lanosrep :: ReversedPersonal
    }

type ReversedTeacher
  = { sedarg :: Array Grade
    , stnedutSfOrebmun :: Int
    , lanosrep :: ReversedPersonal
    , sutats :: TeachingStatus
    }

teacher :: Teacher
teacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal:
      { height: Centimeters 162.56
      , weight: Kilograms 63.5
      , age: Years 31
      }
  , status: NonTenured
  }

decodeOptions :: Options
decodeOptions =
  defaultOptions
    { sumEncoding =
      TaggedObject
        { tagFieldName: "gat"
        , contentsFieldName: "stnetnoc"
        , constructorTagTransform: identity
        }
    }

processAjaxResult ::
  ??? a.
  Show a =>
  Decode a =>
  Proxy a ->
  Either Ajax.Error (Ajax.Response String) ->
  String
processAjaxResult _ = case _ of
  Left err -> Ajax.printError err
  Right { body } -> do
    case runExcept (decodeJSON body :: _ a) of
      Left err -> show err
      Right reversedRecord -> show reversedRecord

test :: Effect Unit
test =
  launchAff_ do
    result <-
      Ajax.post ResponseFormat.string "http://localhost:3000/"
        $ Just
        $ RequestBody.String
        $ encodeJSON teacher
    log
      $ processAjaxResult (Proxy :: _ ReversedTeacher) result
