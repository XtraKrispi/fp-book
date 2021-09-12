module Ch6 where

import Data.Generic.Rep (class Generic)
import Prelude
import Data.Show.Generic (genericShow)
import Data.Newtype

data Place
  = First
  | Second
  | Third

derive instance eqPlace :: Eq Place
derive instance ordPlace :: Ord Place

derive instance genericPlace :: Generic Place _
instance showPlace :: Show Place where
  show = genericShow

newtype Person
  = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

type Address
  = { street1 :: String
    , street2 :: String
    , city :: String
    , state :: String
    , zip :: String
    }

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

newtype Ceo
  = Ceo Person

genericPersonHasAddress ::
  ∀ a.
  Newtype a Person =>
  a ->
  Address
genericPersonHasAddress = getAddress <<< unwrap

derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor
  = Janitor Person

derive instance newtypeJanitor :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor
