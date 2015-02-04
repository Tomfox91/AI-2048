module Board where

import Prelude            hiding (Left, Right)
import Data.List          (transpose)
import Data.Bits
import qualified Data.Vector.Unboxed as V
import Text.Printf        (printf)
import Control.Parallel.Strategies (NFData)
import Debug.Trace        (trace)


type Row = [Int]
type Board = [Row]

type TileT = Int
type RowT = (TileT, TileT, TileT, TileT)
type BoardT = (RowT, RowT, RowT, RowT)

rk :: Int -> TileT
rk     0 =  0
rk     2 =  1
rk     4 =  2
rk     8 =  3
rk    16 =  4
rk    32 =  5
rk    64 =  6
rk   128 =  7
rk   256 =  8
rk   512 =  9
rk  1024 = 10
rk  2048 = 11
rk  4096 = 12
rk  8192 = 13
rk 16384 = 14
rk 32768 = 15
rk 65536 = 16

num :: TileT -> Int
num  0 =     0
num  1 =     2
num  2 =     4
num  3 =     8
num  4 =    16
num  5 =    32
num  6 =    64
num  7 =   128
num  8 =   256
num  9 =   512
num 10 =  1024
num 11 =  2048
num 12 =  4096
num 13 =  8192
num 14 = 16384
num 15 = 32768
num 16 = 65536

toRowT :: Row -> RowT
toRowT [a,b,c,d] = (rk a, rk b, rk c, rk d)

toBoardT :: Board -> BoardT
toBoardT [a,b,c,d] = (toRowT a, toRowT b, toRowT c, toRowT d)

toDirection :: Int -> Direction
toDirection 0 = Up
toDirection 1 = Right
toDirection 2 = Down
toDirection 3 = Left

toRow :: RowT -> Row
toRow (a,b,c,d) = [num a, num b, num c, num d]

toBoard :: BoardT -> Board
toBoard (a,b,c,d) = [toRow a, toRow b, toRow c, toRow d]

intToRowT :: Int -> RowT
intToRowT i = (
		(i `shift` (-12) .&. 0x0F),
		(i `shift` (-08) .&. 0x0F),
		(i `shift` (-04) .&. 0x0F),
		(i               .&. 0x0F)
	)

rowTToInt :: RowT -> Int
rowTToInt (a,b,c,d) = 
	a `shift` 12 .|.
	b `shift`  8 .|.
	c `shift`  4 .|.
	d


emptyBoard = toBoardT [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]]

data Direction = Up | Right | Down | Left deriving (Eq, Show, Read)
instance NFData Direction
directions = [Up, Right, Down, Left]

precompute :: (V.Unbox a) => (Row -> a) -> (RowT -> a)
precompute f = 
	let	vec = V.generate (rowTToInt (15,15,15,15)) (f . toRow . intToRowT)
	in	(V.unsafeIndex vec . rowTToInt)

reverseT :: (t, t, t, t) -> (t, t, t, t)
reverseT (a,b,c,d) = (d,c,b,a)

mapT :: (t -> t1) -> (t, t, t, t) -> (t1, t1, t1, t1)
mapT f (a,b,c,d) = (f a,f b,f c,f d)

sumT :: (Num t) => (t,t,t,t) -> t
sumT (a,b,c,d) = a + b + c + d

transposeT (
		(a1,a2,a3,a4),
		(b1,b2,b3,b4),
		(c1,c2,c3,c4),
		(d1,d2,d3,d4)
	) = (
		(a1,b1,c1,d1),
		(a2,b2,c2,d2),
		(a3,b3,c3,d3),
		(a4,b4,c4,d4)
	)




















