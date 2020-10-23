module Lib
    (   Exreal(Inf, Infsimal, Ju, Negate),
        Hyper(J, L), Hyperreal, hMap, n, m, el, l
    ) where

data Exreal a = Inf | Infsimal | Ju a | Negate (Exreal a)

data Hyper a = J a | L [a]
type Hyperreal = Hyper Double

instance Show t => Show (Hyper t) where
    show (J a) = show a
    show (L xs) = show (take 10 xs) ++ "..."

instance Eq t => Eq (Hyper t) where
    (J a) == (J b) = a == b
    (J a) == (L b) = hP ( == a) (L b)
    (L a) == (J b) = hP ( == b) (L a)
    (L a) == (L b) = all id (zipWith (==) a b)

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

hP :: (t -> Bool) -> Hyper t -> Bool
hP p (J a) = p a
hP p (L xs) = all p (take 10 (dropWhile (\x -> not (p x)) xs))

hMap :: (t -> t) -> Hyper t  -> Hyper t 
hMap f (J a) = J (f a)
hMap f (L xs) = L (map f xs)


n :: Hyperreal
n = L [1..]

m :: Hyperreal
m = L $ iterate (*10) 1

el :: Hyperreal
el = hMap (1/) m

l :: a -> [a]
l = iterate id
one = l 1
