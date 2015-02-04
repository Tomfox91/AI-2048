module Rand.Solver where

import Board
import GameBase
import System.Random
import System.IO
import Prelude hiding (Left, Right)

type FunState = (Int, StdGen)

fChoice :: FunState -> BoardT -> IO (Direction, FunState)
fChoice (move, gen) b = do
	let	(rn, ngen) = randomR (0, 3) gen :: (Int, StdGen)
		d 0 = Up
		d 1 = Right
		d 2 = Down
		d 3 = Left

	putStrLn $ "Move: " ++ (show move)

	return (d rn, (move+1, ngen))


solver :: IO ()
solver = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stdin  NoBuffering
	gen <- getStdGen
	game (0, gen) fChoice
























