module Vector where

import Data.Kind (Type)
import Natural
import Data.Data

infixr 5 :+
data Vect :: Nat -> Type -> Type where
  VNil :: Vect 'Z a
  (:+) :: a -> Vect n a -> Vect ('S n) a
  

  
toList :: Vect n a -> [a]
toList VNil = []
toList (x :+ xs) = x : toList xs

fromList :: SNat n -> [a] -> Vect n a
fromList SZ     _        = VNil
fromList (SS n) (x : xs) = x :+ fromList n xs
fromList _ _ = error "Index out of range"

instance Functor (Vect n) where
  fmap _ VNil      = VNil
  fmap f (x :+ xs) = f x :+ fmap f xs

instance Applicative (Vect 'Z) where
  pure _ = VNil
  _ <*> _ = VNil

instance (ToSNat n, Applicative (Vect n)) => Applicative (Vect ('S n)) where
  pure x = replicateV (toSNat (Proxy :: Proxy ('S n))) x
  (f :+ fs) <*> (x :+ xs) = f x :+ (fs <*> xs)

instance Show a => Show (Vect n a) where
  show ls = show $ toList ls
    
data Fin :: Nat -> Type where
  FZ :: Fin ('S k)
  FS :: Fin k -> Fin ('S k)

fin2Nat :: Fin n -> Nat
fin2Nat FZ = Z
fin2Nat (FS f) = S (fin2Nat f)

fin2Int :: Fin n -> Int
fin2Int FZ     = 0
fin2Int (FS f) = 1 + fin2Int f

sNat2Fin :: SNat n -> n :<: m -> Fin m
sNat2Fin SZ     LZ      = FZ
sNat2Fin (SS n) (LS nm) = FS (sNat2Fin n nm)

instance (FromNat n) => Show (Fin n) where
  show f = "f" ++ (show . nat2Int . fin2Nat) f ++ "{" ++ show (fromNat (Proxy :: Proxy n)) ++ "}"

zipWithV :: (Applicative (Vect m)) => (a -> b -> c) -> Vect m a -> Vect m b -> Vect m c
zipWithV f v1 v2 = f <$> v1 <*> v2 

zipV :: Vect n a -> Vect n b -> Vect n (a, b)
zipV VNil    VNil    = VNil
zipV (x:+xs) (y:+ys) = (x, y) :+ zipV xs ys

replicateV :: SNat n -> a -> Vect n a
replicateV SZ     _ = VNil
replicateV (SS n) x = x :+ replicateV n x

headV :: Vect ('S n) a -> a
headV (x :+ _) = x

tailV :: Vect ('S n) a -> Vect n a
tailV (_ :+ xs) = xs

sumV :: Num a => Vect n a -> a
sumV VNil = 0
sumV (x :+ xs) = x + sumV xs

productV :: Num a => Vect n a -> a
productV VNil = 1
productV (x :+ xs) = x * productV xs

infixl 9 !
(!) :: Vect n a -> Fin n -> a
(x :+ _)  ! FZ     = x
(_ :+ xs) ! (FS f) = xs ! f

infixl 9 !#
(!#) :: Vect n a -> Nat -> a
VNil      !# _     = error "Index out of range"
(x :+  _) !# Z     = x
(_ :+ xs) !# (S n) = xs !# n

fins :: SNat n -> Vect n (Fin n)
fins SZ     = VNil
fins (SS f) = FZ :+ fmap FS (fins f)

testVec :: Vect ('S ('S 'Z)) Nat
testVec = 1 :+ 2 :+ VNil







