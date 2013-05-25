import qualified Data.Char as C

data FormData a = FormData String deriving Show

data Validated
data Unvalidated
 
-- since we don't export the constructor itself,
-- users with a String can only create Unvalidated values
formData :: String -> FormData Unvalidated
formData str = FormData str
 
class Sanitise a where
  sanitise :: FormData a -> FormData Validated
 
-- do nothing to data that is already validated
instance Sanitise Validated where
  sanitise = id
 
-- sanitise untrusted data
instance Sanitise Unvalidated where
  sanitise (FormData str) = FormData (filter C.isAlpha str)
