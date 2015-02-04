import Board
import GameBase
import Prelude hiding (Left, Right)
import System.Random
import System.IO


fChoice :: () -> BoardT -> IO (Direction, ())
fChoice _ b = do
	putStr " > "
	c <- getChar
	putStr "\n"
	let	tr 'w' = Up
		tr 'a' = Left
		tr 's' = Down
		tr 'd' = Right
		dir = tr c
	return (dir, ())

main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stdin  NoBuffering
	game () fChoice




