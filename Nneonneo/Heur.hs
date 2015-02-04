module Nneonneo.Heur 	where

import Board


h0 :: Row -> Int
h0 r = 
	let	c0 []     = 0
		c0 (0:tl) = 1 + c0 tl
		c0 (_:tl) =     c0 tl
	in	10 * c0 r

hBegEnd :: Row -> Int
hBegEnd r =
	let	mx = maximum r
	in	if (mx == head r) || (mx == last r) then 20 else 0

hDoub :: Row -> Int
hDoub r = 
	let	dbl (x:y:tl)
			| x == 2*y || y == 2*x = 1 + dbl (y:tl)
			| otherwise            =     dbl (y:tl)
		dbl _ = 0
	in	1 * dbl r

hSort :: Row -> Int
hSort r =
	let	isSortedI (x:y:tl)
			| x < y     = isSortedI (y:tl)
			| otherwise = False
		isSortedI _ = True
		isSortedD (x:y:tl)
			| x > y     = isSortedD (y:tl)
			| otherwise = False
		isSortedD _ = True
		isSorted r = isSortedD r || isSortedI r
	in	if (isSorted r) then 10 else 0


rowHeur :: Row -> Float
rowHeur r = (1000.0*) . fromIntegral $ h0 r + hBegEnd r + hDoub r + hSort r

pcrh :: RowT -> Float
pcrh = precompute rowHeur

heur :: BoardT -> Float
heur b = (sumT . mapT pcrh $ b)
	   + (sumT . mapT pcrh . transposeT $ b)















