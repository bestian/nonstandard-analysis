import Ratio

c :: [Double]
c = [1..]

inf = 1/0

m :: [Double]
m = iterate (*10) 1
--m = map (\x -> 10 ^ x) c10

el :: [Double]
el = map (1/) m
--el = map (\x -> 1/ (10^x)) m

pw :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
pw o x y = map (\k -> (x!!k) `o` (y!!k)) [0..]
pw2 = pw

pw1 f x = map (\k -> f (x!!fromIntegral k)) [0..] 



l :: a -> [a]
l = iterate id
one = l 1


st x = case x of 
		[] -> 100
		c -> inf
		otherwise -> x !! 100000

part = take 10
far = takeWhile (\x -> (x + 1/x) < 10^100)
eq x y =  part x == part y



geo [0] = one

{-
geo [1] = m
geo [2,3] = 2*m*3*m
geo [4,5,6] = 4*m*5*m*6*m

-}

main = do
		print " Compeletes:"
		print " part c:"
		print $ part c
		print " part m:"
		print $ part m
		print " part el:"
		print $ part el
		print " part $ pw (*) m el"
		print $ part $ pw (*) m el


{-
c k = pw1 (\x -> (logBase k 10) * x) c10

-}


{-
main = do
		print " Compeletes:"
		print ""
		print ""
		print " take 10 c10"
		print $ take 10 c10
		print ""
		print " take 10 m"
		print $ take 10 m
		print ""
		print " take 10 (pw logBase (l 10) m)"
		print $ take 10 (pw logBase (l 10) m)
		print ""
		print " (pw logBase (l 10) m) `eq` c10"
		print $ (pw logBase (l 10) m) `eq` c10
		print ""
		print " take 10 (c 3)"
		print $ take 10 (c 3)
		print ""
		print " take 10 $ pw to (l 3) (c 3)"
		print $ take 10 $ pw to (l 3) (c 3)
		print " ERRORs/TODOs:"
		print ""
		print ""
		print " pw to (l 3) (c 3) `eq` m"
		print $ pw to (l 3) (c 3) `eq` m
		print ""
		print " take 50 (pw logBase (l 10) m)"
		print $ take 50 (pw logBase (l 10) m)

	-}