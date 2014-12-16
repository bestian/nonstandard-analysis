import Ratio

c10 = [1..]

m = map (\x -> 10 ^ x) c10


plus a b = a + b
minus a b = a - b
times a b = a * b
divides a b = a / b
to a b = a ^ b

pw o x y = map (\k -> (x!!k) `o` (y!!k)) [0..]
pw2 = pw


pw1 f x = map (\k -> f (x!!fromIntegral k)) [0..] 

l :: a -> [a]
l = iterate id


eq x y = take 10 x == take 10 y

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