{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
module Matrix where

import Vector
import Natural
import Data.Kind(Type)
import Data.Data
import Control.Applicative (liftA2)
import Boolean

data Mat :: Nat -> Nat -> Type -> Type where
  Mat :: Vect m (Vect n a) -> Mat m n a

runMat :: Mat m n a -> Vect m (Vect n a)
runMat (Mat m) = m

instance Show a => Show (Mat m n a) where
  show (Mat mat) = show mat

instance Functor (Mat m n) where
  fmap f (Mat mat) = Mat $ fmap (fmap f) mat

instance Applicative (Mat 'Z n) where
  pure _ = Mat VNil
  _ <*> _ = Mat VNil

instance (ToSNat m, ToSNat n, Applicative (Mat m n), Applicative (Vect n)) => Applicative (Mat ('S m) n) where
  pure x = Mat $ replicateV (toSNat Proxy) $ replicateV (toSNat Proxy) x
  Mat (f :+ fs) <*> Mat (x :+ xs) = Mat $ (f <*> x) :+ runMat (Mat fs <*> Mat xs)

list2Mat :: SNat m -> SNat n -> [[a]] -> Mat m n a
list2Mat SZ     _ _        = Mat VNil
list2Mat (SS m) n (l : ls) = Mat $ fromList n l :+ runMat (list2Mat m n ls)
list2Mat _ _ _ = error "Index out of range"

instance (Applicative (Mat m n), Num a) => Num (Mat m n a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

class Transpose (mat :: Nat -> Nat -> Type -> Type) (m :: Nat) (n :: Nat) where
  transpose :: mat m n a -> mat n m a

instance ToSNat n => Transpose Mat 'Z n  where
  transpose (Mat VNil) = Mat $ replicateV (toSNat (Proxy :: Proxy n)) VNil

instance (Applicative (Vect n), (Transpose Mat m n)) => Transpose Mat ('S m) n where
  transpose (Mat (x :+ xs)) = Mat $ zipWithV (:+) x (runMat $ transpose (Mat xs)) 

infixl 7 .@.
(.@.) :: forall {m :: Nat} {n :: Nat} {a :: Type} . (Num a, Applicative (Vect n)) => Mat m n a -> Vect n a -> Vect m a
Mat mat .@. vect = fmap f mat
  where
    f :: Vect n a -> a
    f vect' = sumV $ (*) <$> vect' <*> vect 

zipM :: Mat m n a -> Mat m n b -> Mat m n (a, b)
zipM (Mat VNil)    (Mat VNil)    = Mat VNil
zipM (Mat (x:+xs)) (Mat (y:+ys)) = Mat $ (zipV x y) :+ runMat (zipM (Mat xs) (Mat ys)) 
    
rowHead :: Mat ('S m) n a -> Vect n a
rowHead (Mat (h :+ _)) = h

rowTail :: Mat ('S m) n a -> Mat m n a
rowTail (Mat (_ :+ t)) = Mat t

colHead :: Mat m ('S n) a -> Vect m a
colHead = fmap headV . runMat

colTail :: Mat m ('S n) a -> Mat m n a
colTail = Mat . fmap tailV . runMat

rowMat :: Vect n a -> Mat T1 n a
rowMat = Mat . (:+ VNil)

colMat :: (ToSNat m, Applicative (Vect m)) => Vect m a -> Mat m T1 a
colMat = transpose . rowMat

colConsMat :: Applicative (Vect m) => Vect m a -> Mat m n a -> Mat m ('S n) a
colConsMat v (Mat mat) = Mat $ (:+) <$> v <*> mat

rowConsMat :: Vect n a -> Mat m n a -> Mat ('S m) n a
rowConsMat v (Mat mat) = Mat $ v :+ mat

infixr 5 |:, -:
(|:) :: Applicative (Vect m) => Vect m a -> Mat m n a -> Mat m ('S n) a
(|:) = colConsMat
(-:) :: Vect n a -> Mat m n a -> Mat ('S m) n a
(-:) = rowConsMat

infixl 7 .*.
class MatMult (m :: Nat) (l :: Nat) (n :: Nat) where
  (.*.) :: (Num a) => Mat m l a -> Mat l n a -> Mat m n a

instance Applicative (Vect m) => MatMult m l 'Z where
  _ .*. _ = Mat $ pure VNil

instance (Applicative (Vect l), Applicative (Vect m), MatMult m l n) => MatMult m l ('S n) where
  m1 .*. m2 =  m1 .@. colHead m2 |: m1 .*. colTail m2

deleteRow :: Mat ('S m) n a -> Fin ('S m) -> Mat m n a
deleteRow (Mat (_ :+ xs))      FZ     = Mat xs
deleteRow (Mat (x :+ y :+ ls)) (FS f) = x -: deleteRow (Mat (y :+ ls)) f
deleteRow _ _ = error "SHUT UP" -- inaccessible

class DeleteCol (m :: Nat) (n :: Nat) where
  deleteCol :: Mat m ('S n) a -> Fin ('S n) -> Mat m n a

instance DeleteCol m 'Z where
  deleteCol mat FZ = colTail mat
  deleteCol _ _ = error "SHUT UP" -- inaccessible

instance (Applicative (Vect m), DeleteCol m n) => DeleteCol m ('S n) where
  deleteCol mat FZ     = colTail mat
  deleteCol mat (FS f) = colHead mat |: deleteCol (colTail mat) f

-- 子阵
subMat :: (DeleteCol m n) => Mat ('S m) ('S n) a -> Fin ('S m) -> Fin ('S n) -> Mat m n a
subMat mat fr fc = deleteCol (deleteRow mat fr) fc

altSum :: Num a => Vect n a -> a
altSum (x:+y:+ls) = (x - y) + altSum ls
altSum (x:+VNil) = x
altSum VNil = 0

-- 行列式
class Det (m :: Nat) where
  det :: Num a => Mat m m a -> a

instance Det 'Z where
  det _ = 1

instance (DeleteCol m m, Det m, ToSNat m) => Det ('S m) where
  det mat = altSum $ fmap (\f -> (runMat mat ! FZ ! f) * det (subMat mat FZ f)) (fins $ toSNat Proxy)

finMat :: (ToSNat n) => SNat m -> SNat n -> Mat m n (Fin m, Fin n)
finMat SZ     _ = Mat VNil
finMat (SS m) n = Mat $ (zipV (replicateV (toSNat Proxy) FZ ) (fins (toSNat Proxy))) :+ runMat (fmap (\(x, y) -> (FS x, y)) (finMat m n))

-- 伴随矩阵的转置
class Cmp (m :: Nat) where
  cmp :: Num a => Mat m m a -> Mat m m a

instance Cmp 'Z where
  cmp _ = Mat VNil

instance (Det m, ToSNat m, DeleteCol m m) => Cmp ('S m) where
  cmp mat = fmap (\(fx, fy) -> alt fx fy (det $ subMat mat fx fy)) (finMat (toSNat Proxy) (toSNat Proxy))
    where
      alt :: Num a => Fin ('S m) -> Fin ('S m) -> a -> a
      alt fx fy a = if (fin2Int fx + fin2Int fy) `mod` 2 == 0
        then a else - a
   
-- 伴随矩阵   
adj :: (Transpose Mat m m, Cmp m) => Num a => Mat m m a -> Mat m m a
adj = transpose . cmp

-- 逆矩阵
inv :: (Det m, Cmp m, Transpose Mat m m, Fractional a) => Mat m m a -> Mat m m a
inv mat = fmap (/ det mat) $ adj mat

testMat :: Num a => Mat T3 T2 a
testMat = list2Mat s3 s2 $
  [ [1, 2]
  , [3, 4]
  , [5, 6]]

testMat2 :: Num a => Mat T2 T4 a
testMat2 = list2Mat s2 s4 $
  [ [1, 2, 3, 4]
  , [5, 6, 7, 8]]

testMat3 :: Num a => Mat T3 T3 a
testMat3 = list2Mat s3 s3 $
  [ [2, 4, 4]
  , [4, 4, 5]
  , [5, 4, 7]]

fMat :: Num a => Mat T3 T2 (a -> a)
fMat = list2Mat s3 s2 $
  [ [(+1), (+2)]
  , [(+3), (+4)]
  , [(+5), (+6)]]