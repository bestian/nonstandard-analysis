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


hMap :: (Double -> Double) -> Hyperreal -> Hyperreal
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
