module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (&&), (==))

class Semigroup a where
  append :: a -> a -> a

class
  Semigroup a <= Monoid a where
  mempty :: a

infixr 5 append as <>

class
  Monoid a <= Group a where
  ginverse :: a -> a

data AndBool
  = AFalse
  | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ (AFalse <> (ATrue <> ATrue)) == ((AFalse <> ATrue) <> ATrue)

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ ATrue <> mempty == mempty <> ATrue && mempty <> ATrue == ATrue
  log $ show $ AFalse <> mempty == mempty <> AFalse && mempty <> AFalse == AFalse

data OrBool
  = OFalse
  | OTrue
derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

data Mod4
  = Zero
  | One
  | Two
  | Three
derive instance eqMod4 :: Eq Mod4
derive instance genericMod4 :: Generic Mod4 _

instance showMod4 :: Show Mod4 where
  show = genericShow

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero any = any
  append any Zero = any
  append One One = Two
  append One Two = Three
  append One Three = Zero
  append Two One = Three
  append Two Two = Zero
  append Two Three = One
  append Three One = Zero
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One

newtype First a
  = First (Maybe a)
newtype Last a
  = Last (Maybe a)

derive instance genericFirst :: Generic (First a) _
derive instance genericLast :: Generic (Last a) _

instance showFirst :: Show a => Show (First a) where
  show = genericShow

instance showLast :: Show a => Show (Last a) where
  show = genericShow

instance semigroupFirst :: Semigroup (First a) where
  append (First Nothing) x = x
  append x _ = x

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

instance semigroupLast :: Semigroup (Last a) where
  append x (Last Nothing) = x
  append _ x = x

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> (OTrue <> OTrue)) == ((OFalse <> OTrue) <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws (2 tests)"
  log $ show $ OTrue <> mempty == mempty <> OTrue && mempty <> OTrue == OTrue
  log $ show $ OFalse <> mempty == mempty <> OFalse && mempty <> OFalse == OFalse

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws (1 test)"
  log $ show $ (One <> (Two <> Three)) == ((One <> Two) <> Three)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws (1 test)"
  log $ show $ One <> mempty == mempty <> One && mempty <> One == One

test :: Effect Unit
test = do
  log $ show $ ATrue <> ATrue
  log $ show $ ATrue <> AFalse
  log $ show $ AFalse <> AFalse
  log $ show $ mempty <> ATrue == ATrue
  log $ show $ mempty <> AFalse == ATrue
  verifyAndBoolSemigroup
  verifyAndBoolMonoid
  verifyOrBoolSemigroup
  verifyOrBoolMonoid
  verifyMod4Semigroup
  verifyMod4Monoid
  log $ show $ First Nothing <> First (Just 77)
  log $ show $ Last (Just 1) <> Last (Just 99)
