module Natural where

import Data.Kind (Type)
import Data.Data
import Control.Monad (forM_)

data Nat = Z | S Nat deriving Eq

nat2Int :: Nat -> Int
nat2Int Z = 0
nat2Int (S n) = 1 + nat2Int n

instance Show Nat where
  show = ("n"++) . show . nat2Int

instance Num Nat where
  Z     + n = n
  (S m) + n = S (m + n)

  Z     - _     = Z
  m     - Z     = m
  (S m) - (S n) = m - n

  Z     * _ = Z
  (S m) * n = n + (m * n)

  fromInteger 0 = Z
  fromInteger n = S $ fromInteger (n - 1)

  abs x = x
  signum _ = 1

data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

infixl 6 |+|
(|+|) :: SNat m -> SNat n -> SNat (m :+: n)
SZ   |+| n = n
SS m |+| n = SS (m |+| n)

infixl 7 |*|
(|*|) :: SNat m -> SNat n -> SNat (m :*: n)
SZ   |*| _ = SZ
SS m |*| n = n |+| (m |*| n)


type family (:+:) (m :: Nat) (n :: Nat) :: Nat where
  'Z     :+: n = n
  ('S m) :+: n = 'S (m :+: n)

type family (:*:) (m :: Nat) (n :: Nat) :: Nat where
  'Z     :*: _ = 'Z
  ('S m) :*: n = n :+: (m :*: n)
  
  

class FromNat (nat :: Nat) where
  fromNat :: Proxy nat -> Nat 

instance FromNat 'Z where
  fromNat _ = 0 
  
instance FromNat n => FromNat ('S n) where
  fromNat _ = S $ fromNat (Proxy :: Proxy n)

class ToSNat (nat :: Nat) where
  toSNat :: Proxy nat -> SNat nat
  
instance ToSNat 'Z where
  toSNat Proxy = SZ
  
instance ToSNat n => ToSNat ('S n) where
  toSNat Proxy = SS (toSNat (Proxy :: Proxy n))

type T0 = 'Z
type T1 = 'S T0
type T2 = 'S T1
type T3 = 'S T2
type T4 = 'S T3
type T5 = 'S T4
type T6 = 'S T5
type T7 = 'S T6
type T8 = 'S T7
type T9 = 'S T8
type T10 = 'S T9
type T11 = 'S T10
type T12 = 'S T11
type T13 = 'S T12
type T14 = 'S T13
type T15 = 'S T14
type T16 = 'S T15
type T17 = 'S T16
type T18 = 'S T17
type T19 = 'S T18
type T20 = 'S T19
type T21 = 'S T20
type T22 = 'S T21
type T23 = 'S T22
type T24 = 'S T23
type T25 = 'S T24
type T26 = 'S T25
type T27 = 'S T26
type T28 = 'S T27
type T29 = 'S T28
type T30 = 'S T29
type T31 = 'S T30
type T32 = 'S T31
type T33 = 'S T32
type T34 = 'S T33
type T35 = 'S T34
type T36 = 'S T35
type T37 = 'S T36
type T38 = 'S T37
type T39 = 'S T38
type T40 = 'S T39
type T41 = 'S T40
type T42 = 'S T41
type T43 = 'S T42
type T44 = 'S T43
type T45 = 'S T44
type T46 = 'S T45
type T47 = 'S T46
type T48 = 'S T47
type T49 = 'S T48
type T50 = 'S T49
type T51 = 'S T50
type T52 = 'S T51
type T53 = 'S T52
type T54 = 'S T53
type T55 = 'S T54
type T56 = 'S T55
type T57 = 'S T56
type T58 = 'S T57
type T59 = 'S T58
type T60 = 'S T59
type T61 = 'S T60
type T62 = 'S T61
type T63 = 'S T62

s0 :: SNat T0
s0 = SZ
s1 :: SNat T1
s1 = SS s0
s2 :: SNat T2
s2 = SS s1
s3 :: SNat T3
s3 = SS s2
s4 :: SNat T4
s4 = SS s3
s5 :: SNat T5
s5 = SS s4
s6 :: SNat T6
s6 = SS s5
s7 :: SNat T7
s7 = SS s6
s8 :: SNat T8
s8 = SS s7
s9 :: SNat T9
s9 = SS s8
s10 :: SNat T10
s10 = SS s9
s11 :: SNat T11
s11 = SS s10
s12 :: SNat T12
s12 = SS s11
s13 :: SNat T13
s13 = SS s12
s14 :: SNat T14
s14 = SS s13
s15 :: SNat T15
s15 = SS s14
s16 :: SNat T16
s16 = SS s15
s17 :: SNat T17
s17 = SS s16
s18 :: SNat T18
s18 = SS s17
s19 :: SNat T19
s19 = SS s18
s20 :: SNat T20
s20 = SS s19
s21 :: SNat T21
s21 = SS s20
s22 :: SNat T22
s22 = SS s21
s23 :: SNat T23
s23 = SS s22
s24 :: SNat T24
s24 = SS s23
s25 :: SNat T25
s25 = SS s24
s26 :: SNat T26
s26 = SS s25
s27 :: SNat T27
s27 = SS s26
s28 :: SNat T28
s28 = SS s27
s29 :: SNat T29
s29 = SS s28
s30 :: SNat T30
s30 = SS s29
s31 :: SNat T31
s31 = SS s30
s32 :: SNat T32
s32 = SS s31
s33 :: SNat T33
s33 = SS s32
s34 :: SNat T34
s34 = SS s33
s35 :: SNat T35
s35 = SS s34
s36 :: SNat T36
s36 = SS s35
s37 :: SNat T37
s37 = SS s36
s38 :: SNat T38
s38 = SS s37
s39 :: SNat T39
s39 = SS s38
s40 :: SNat T40
s40 = SS s39
s41 :: SNat T41
s41 = SS s40
s42 :: SNat T42
s42 = SS s41
s43 :: SNat T43
s43 = SS s42
s44 :: SNat T44
s44 = SS s43
s45 :: SNat T45
s45 = SS s44
s46 :: SNat T46
s46 = SS s45
s47 :: SNat T47
s47 = SS s46
s48 :: SNat T48
s48 = SS s47
s49 :: SNat T49
s49 = SS s48
s50 :: SNat T50
s50 = SS s49
s51 :: SNat T51
s51 = SS s50
s52 :: SNat T52
s52 = SS s51
s53 :: SNat T53
s53 = SS s52
s54 :: SNat T54
s54 = SS s53
s55 :: SNat T55
s55 = SS s54
s56 :: SNat T56
s56 = SS s55
s57 :: SNat T57
s57 = SS s56
s58 :: SNat T58
s58 = SS s57
s59 :: SNat T59
s59 = SS s58
s60 :: SNat T60
s60 = SS s59
s61 :: SNat T61
s61 = SS s60
s62 :: SNat T62
s62 = SS s61
s63 :: SNat T63
s63 = SS s62

data (m :: Nat) :<: (n :: Nat) :: Type where
  LZ :: 'Z :<: 'S n
  LS :: m :<: n -> 'S m :<: 'S n
