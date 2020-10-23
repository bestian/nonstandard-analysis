module Lib
    (   Ex(Inf, Infsimal, Ju, Negate), Exreal,
        Hyper(J, L), Hyperreal, hP, hMap, n, m, el
    ) where

data Num a => Ex a = Inf | Infsimal | Ju a | Negate (Ex a) deriving Show
type Exreal = Ex Double

instance (Eq a, Num a) => Eq (Ex a) where
    Inf == Inf = True
    Infsimal == Infsimal = True
    Ju a == Ju b = a == b
    Negate x == Negate y = x == y
    _ == _ = False

instance (Ord a, Num a) => Ord (Ex a) where
    Inf `compare` Inf = EQ
    Inf `compare` _ = GT
    Infsimal `compare` Infsimal = EQ
    Infsimal `compare` (Ju a) | a > 0     = LT
                              | a == 0    = GT
                              | otherwise = GT
    (Ju a) `compare` Infsimal | a < 0     = LT
                              | a == 0    = LT
                              | otherwise = GT
    Infsimal `compare` (Negate _) = GT
    Negate Infsimal `compare` (Negate Infsimal) = EQ
    Negate Infsimal `compare` (Negate _) = GT
    Negate x `compare` Negate y | x > y  = LT
                                | x < y  = GT
                                | otherwise = EQ


data Hyper a = J a | L [a]
type Hyperreal = Hyper Double

instance Show t => Show (Hyper t) where
    show (J a) = show a
    show (L xs) = show (take 10 xs) ++ "..."

instance Eq t => Eq (Hyper t) where
    (J a) == (J b) = a == b
    (J a) == (L b) = hP ( == a) (L b)
    (L a) == (J b) = hP ( == b) (L a)
    (L a) == (L b) = hP id (L (zipWith (==) a b))

instance Ord t => Ord (Hyper t) where
    (J a) `compare` (J b) = a `compare` b
    (J a) `compare` (L b) | hP (== GT) or = GT
                          | hP (== LT) or = LT
                          | hP (== EQ) or = EQ
                          | otherwise = error "cannot compare"
        where or = hMap (compare a) (L b)
    (L a) `compare` (J b) | hP (== GT) or = GT
                          | hP (== LT) or = LT
                          | hP (== EQ) or = EQ
                          | otherwise = error "cannot compare"
        where or = hMap (\a -> compare a b) (L a)
    (L a) `compare` (L b) | hP (== GT) or = GT
                          | hP (== LT) or = LT
                          | hP (== EQ) or = EQ
                          | otherwise = error "cannot compare"
        where or = L (zipWith compare a b)

instance Num t => Num (Hyper t) where
    (J a) + (J b) = J (a + b)
    (J a) + (L b) = hMap ( + a) (L b)
    (L a) + (J b) = hMap ( + b) (L a)
    (L a) + (L b) = L $ zipWith (+) a b
    (J a) * (J b) = J (a * b)
    (J a) * (L b) = hMap ( * a) (L b)
    (L a) * (J b) = hMap ( * b) (L a)
    (L a) * (L b) = L $ zipWith (*) a b
    abs (J a) = J (abs a)
    abs (L a) = hMap abs (L a)
    signum (J a) = J (signum a)
    signum (L a) = hMap signum (L a)
    negate (J a) = J (negate a)
    negate (L a) = hMap negate (L a)
    fromInteger a = J (fromInteger a)

instance Fractional t => Fractional (Hyper t) where
    recip (J a) = J (recip a)
    recip (L a) = hMap recip (L a)
    fromRational a = J (fromRational a)
 
hP :: (t -> Bool) -> Hyper t -> Bool
hP p (J a) = p a
hP p (L xs) = all p (take 10 (dropWhile (\x -> not (p x)) xs))

hMap :: (t -> t0) -> Hyper t  -> Hyper t0 
hMap f (J a) = J (f a)
hMap f (L xs) = L (map f xs)


n :: Hyperreal
n = L [1..]

m :: Hyperreal
m = L $ iterate (*10) 1

el :: Hyperreal
el = hMap (1/) m

