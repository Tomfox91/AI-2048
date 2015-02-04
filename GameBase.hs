module GameBase where

import Prelude     hiding (Left, Right)
import Board
import Data.Word
import Text.Printf (printf)
import Debug.Trace (trace)
import System.Random

boardToString :: Board -> String
boardToString b =
	foldl1 (++) $
		map (\r ->
			(foldl1 (++) $
				map (\n -> if (n /= 0) then printf "%5d" n else "    .") r
			) ++ "\n") b

printBoard :: BoardT -> IO ()
printBoard = putStr . boardToString . toBoard

normalizeBoard :: Direction -> BoardT -> BoardT
normalizeBoard Left  b = b
normalizeBoard Right b = mapT reverseT b
normalizeBoard Up    b = transposeT b
normalizeBoard Down  b = mapT reverseT $ transposeT b

deNormalizeBoard :: Direction -> BoardT -> BoardT
deNormalizeBoard Left  b = b
deNormalizeBoard Right b = mapT reverseT b
deNormalizeBoard Up    b = transposeT b
deNormalizeBoard Down  b = transposeT $ mapT reverseT b

-- Calcola il risultato di uno spostamento in una direzione di una board
push :: BoardT -> Direction -> BoardT
push =
	let	mv0 :: Row -> Row
		mv0 [] = []
		mv0 (0:tl) = mv0 tl ++ [0]
		mv0 (x:tl) = x : mv0 tl

		fpush :: Row -> Row
		fpush [] = []
		fpush l@(0:_) = l
		fpush (x:y:tl) 
			| x == y = (2*x) : (fpush $ tl) ++ [0]
		fpush (x:tl) = x : fpush tl

		pcf :: RowT -> RowT
		pcf = precompute (toRowT . fpush . mv0)
 	in	(\b d -> deNormalizeBoard d . (mapT pcf) . normalizeBoard d $ b)

pushCk :: BoardT -> Direction -> Maybe BoardT
pushCk b d = 
	let	nb = push b d
	in	if (nb == b) then Nothing else Just nb


rowsWithRandomTileInserted :: TileT -> RowT -> [RowT]
rowsWithRandomTileInserted n (a,b,c,d) =
		(if (a == 0) then [(n,b,c,d)] else []) ++
		(if (b == 0) then [(a,n,c,d)] else []) ++
		(if (c == 0) then [(a,b,n,d)] else []) ++
		(if (d == 0) then [(a,b,c,n)] else []) ++
		[]

boardsWithRandomTileInserted :: Int -> BoardT -> [BoardT]
boardsWithRandomTileInserted num (r,s,t,u) = 
		(map (\x -> (x,s,t,u)) $ rowsWithRandomTileInserted n r) ++
		(map (\x -> (r,x,t,u)) $ rowsWithRandomTileInserted n s) ++
		(map (\x -> (r,s,x,u)) $ rowsWithRandomTileInserted n t) ++
		(map (\x -> (r,s,t,x)) $ rowsWithRandomTileInserted n u) ++
		[]
	where n = rk num




insertRandomNumber :: StdGen -> BoardT -> (StdGen, BoardT)
insertRandomNumber gen b
	| full      = (gen1, b)
	| otherwise = (gen2, out)
	where
		(rand1, gen1) = randomR (0, 9) gen :: (Int, StdGen)
		numVal = if (rand1 == 9) then 4 else 2
		alts = boardsWithRandomTileInserted numVal b
		full = null alts
		(rand2, gen2) = randomR (0, (length alts) -1) gen1 :: (Int, StdGen)
		out = alts !! rand2

gameEnded :: BoardT -> Bool
gameEnded board = full && not anyRepetition
	where
		b = toBoard board
		alln0 :: Board -> Bool
		alln0 b = all (all (/=0)) b
		full = alln0 b

		repetitionInRow :: Row -> Bool
		repetitionInRow []       = False
		repetitionInRow [_]      = False
		repetitionInRow (a:b:tl) = (a == b) || repetitionInRow (b:tl)
		repetition :: Board -> Bool
		repetition b = any repetitionInRow b
		nb d = toBoard . (normalizeBoard d) . toBoardT
		anyRepetition = (repetition $ nb Left b) || (repetition $ nb Up b)


-- Esegue una mossa della partita
gameIteration :: 
	BoardT -> StdGen -> s -> (s -> BoardT -> IO (Direction, s)) -> IO ()
gameIteration b gen s fChoice = do
	putStrLn ""
	printBoard b
	(dir, ns) <- fChoice s b
	case pushCk b dir of
		Nothing -> do
			putStrLn "Illegal move"
			gameIteration b gen ns fChoice
		Just b -> do
			let	(ngen, nb) = insertRandomNumber gen b
			if gameEnded nb	
				then do
					putStrLn ""
					putStrLn "Game Ended"
					printBoard nb
					return ()
				else gameIteration nb ngen ns fChoice

-- Esegue la partita, interrogando fChoice 
-- ad ogni passo per sapere quale mossa effettuare
game :: s -> (s -> BoardT -> IO (Direction, s)) -> IO ()
game is fChoice = do
	gen <- getStdGen
	let	(gen1, b1) = insertRandomNumber gen emptyBoard
		(gen2, b2) = insertRandomNumber gen1 b1
	gameIteration b2 gen2 is fChoice














