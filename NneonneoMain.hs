{- Per abilitare il multithreading, compilare con

	ghc -O2 -threaded -rtsopts NneonneoMain.hs

ed eseguire con

	./NneonneoMain +RTS -N4 -RTS <limite profondità> <limite probabilità>
-}


import Nneonneo.Solver
import System.IO
import System.Environment


-- parametri: (limite profondità) (limite probabilità)
main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stdin  NoBuffering
	args <- getArgs
	let	maxDepth = read (args !! 0) :: Int
		minProb = read (args !! 1) :: Float
	solver (maxDepth, minProb)