module Boolean where

import Data.Kind (Type)


data SBool :: Bool -> Type where
  STrue  :: SBool 'True
  SFalse :: SBool 'False

type family Not (b :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

sNot :: SBool b -> SBool (Not b)
sNot STrue  = SFalse
sNot SFalse = STrue

