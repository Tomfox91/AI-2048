import Ov3y.Solver
import System.IO
import System.Environment


-- parametri: (limite profondità)
main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stdin  NoBuffering
	args <- getArgs
	let	maxDepth = read (args !! 0) :: Int
	solver maxDepth
