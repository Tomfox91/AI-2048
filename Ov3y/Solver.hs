module Ov3y.Solver where

import Board
import GameBase
import Data.List (maximumBy, foldl')
import Control.Parallel.Strategies 
import System.Random
import System.IO
import Debug.Trace (trace)
import Control.DeepSeq (force)
import Prelude hiding (Left, Right)


--INIZIO SMOOTHNESS

-- Restituisce il valore della proprietà smoothness relativa alla board data
smoothness :: Board -> Int -> Int -> Int
smoothness board x smoothnessValore =
	let
		smoothnessTmp = cicloDue board x 0 smoothnessValore
	in
		if x<3
		then
			smoothness board (x+1) smoothnessTmp
		else
			smoothnessTmp

cicloDue :: Board -> Int -> Int -> Int -> Int
cicloDue board x y smoothnessVal =
	let
		cella = (x,y)
		newSmoothness=
			if isNotFree board cella
			then
				let
					log1= log (fromIntegral (valoreCella board cella) :: Float)
					value =  log1 / log (2)
				in
					direzioneSpostamento 1 smoothnessVal cella  (truncate value) board
			else
				smoothnessVal
	in
		if y<3
		then
			cicloDue board x (y+1) newSmoothness
		else
			newSmoothness


direzioneSpostamento :: Int -> Int -> (Int,Int) -> Int -> Board -> Int
direzioneSpostamento direzione smoothnessVal cella value board =
	let
		vector = vettoreDirezione direzione
		(altro, targetCell) = findFarthestPosition cella vector board
		newSmoothness =
			if isFree board targetCell
			then
				smoothnessVal
			else
				let
					target = valoreCella board targetCell
					targetValue = (log (fromIntegral target :: Float)) / (log (fromIntegral 2 :: Float))
				in
					smoothnessVal - (abs ( value - truncate(targetValue)))
	in
		if direzione<2
		then
			direzioneSpostamento (direzione+1) newSmoothness cella value board
		else
			newSmoothness

--Restituisce data una cella e un vettore direzione la prima cella libera in quella direzione o una cella fuori dai limiti della board se non ve ne sono di libere.
findFarthestPosition :: (Int,Int) -> (Int,Int) -> Board -> ((Int,Int),(Int,Int))
findFarthestPosition cella vector board =
	let
		(previusX, previusY) = cella
		(vectorX, vectorY) = vector
		newCella = ((previusX + vectorX), (previusY + vectorY))
	in	
		if withinBounds newCella && (isFree board newCella)
		then
			findFarthestPosition newCella vector board
		else
			((previusX, previusY), newCella)


withinBounds :: (Int,Int) -> Bool
withinBounds cella =
	let
		(x,y) = cella
	in
		x >= 0 && x < 4 && y >= 0 && y < 4


--FINE SMOOTHNESS



--INIZIO ISLANDS

type RowM = [Bool]
type Marker = [RowM]

-- Restituisce il valore del marker indicato
valoreCellaMarker :: Marker -> (Int, Int) -> Bool
valoreCellaMarker b (x, y) = b !! x !! y

-- Restituisce il valore Islands per la board indicata
islands :: Board -> Int
islands board =
	let
		newIsland = cicloPrimo board 0 0 [[False,False,False,False],[False,False,False,False],[False,False,False,False],[False,False,False,False]]
	in
		newIsland

cicloPrimo :: Board -> Int -> Int -> Marker -> Int
cicloPrimo board x islands marker =
	let
		(islandsTmp,markerTmp) = cicloSecondo board x 0 islands marker
	in
		if x<3
		then
			cicloPrimo board (x+1) islandsTmp markerTmp
		else
			islandsTmp

cicloSecondo :: Board -> Int -> Int -> Int -> Marker -> (Int, Marker)
cicloSecondo board x y islands marker =
	let
		cella = (x,y)
		(newIslands, newMarker) =
			if isNotFree board cella && (valoreCellaMarker marker (x,y)==False)
			then
				(islands+1, (funzioneAnonima board x y (valoreCella board (x,y)) marker))
			else
				(islands, marker)
	in
		if y<3
		then
			cicloSecondo board x (y+1) newIslands newMarker
		else
			(newIslands, newMarker)


funzioneAnonima :: Board -> Int -> Int -> Int -> Marker -> Marker
funzioneAnonima board x y value marker =
	if x >= 0 && x <= 3 && y >= 0 && y <= 3 && (isNotFree board (x,y)) && (valoreCella board (x,y) == value) && (valoreCellaMarker marker (x,y) == False)
	then
		let
			tmpMarker = inserisciElemento (x,y) True marker
		in
			tutteDirezioni board tmpMarker x y value 0
	else
		marker

-- Imposta a true tutti i vicini della cella e i vicini dei vicini e così via finché ve ne sono
tutteDirezioni :: Board -> Marker -> Int -> Int -> Int -> Int -> Marker
tutteDirezioni board marker x y value direzione =
	if direzione<4
	then
		let
			(newX, newY) = vettoreDirezione direzione
			subMarker = funzioneAnonima board (x+newX) (y+newY) value marker
		in
			tutteDirezioni board subMarker x y value (direzione+1)
	else
		marker

--FINE ISLANDS

--INIZIO CALCOLO MATTONELLA DA INSERIRE DA PARTE DEL COMPUTER

singolaCella :: Board -> Int -> [(Int,Int)] -> Int -> ([Board], Int)
singolaCella board numIns celleLibere massimo =
	if celleLibere /= []
		then
			let
				newBoard= inserisciElemento (head celleLibere) (2^numIns) board
				val1=smoothness newBoard 0 0
				val2=islands(newBoard)
				valore = -val1 + val2
			in
				if valore>=massimo
				then
					let
						(lista,max) = singolaCella board numIns (tail celleLibere) valore
					in
						if valore >= max
						then
							(newBoard : lista, valore)
						else
							(lista,max)
				else
					singolaCella board numIns (tail celleLibere) massimo
	else
		([], massimo)


inserimenti :: Board -> Int -> [(Int,Int)] -> Int -> ([Board],Int)
inserimenti board numIns celleLibere attualeNumero
	| attualeNumero <= numIns =
		let
		(lista, max1)=singolaCella board attualeNumero celleLibere (-1000000)
		(resto, max2)=inserimenti board numIns celleLibere (attualeNumero+1)
		in
			if max1==max2
			then
				(lista ++ resto,max1)
			else
				if max1>max2
				then
					(lista ++ [],max1)
				else
					(resto,max2)
	| otherwise =
		([],-1000000)

-- Restituisce una lista di board. Ogni board si discosta dall'originale per l'inserimento di un nuovo valore casuale in una casella precedentemente vuota. Le board restituite non sono tutte le possibili board generabili secondo questi criteri ma sono quelle che presentano un valore di Islands massimo, in tale modo si riduce il branch dell'albero mantenendo solo i nodi con più probabilità di minimizzare il valore della euristica generale evaluation
valoriOttimi :: Board -> Int -> [Board]
valoriOttimi board numIns =
	let
		celleLibere = freeCell board
		(listaPapabili,massimo)=inserimenti board numIns celleLibere 1
	in
		listaPapabili

-- Continua la ricerca estendendo l'albero generato sinora con l'aggiunta di nuovi nodi per l'albero minimax. Restituisce poi il miglior valore (per la mossa del computer) tra quelli ritornati dai nodi figli.
inserimentoMigliore :: Int -> Float -> Float -> Int -> Int -> Int -> [Board] -> ((Int, Float,Int,Int),Bool)
inserimentoMigliore depth alpha bestScore positions cutoffs bestMove listaPapabili =
	if listaPapabili/=[]
	then
		let
			(subMove1, subScore1, subPosition1, subCutoffs1) = search depth alpha bestScore (positions+1) cutoffs True (head listaPapabili)
			bestScoreTmp = 
				if subScore1<bestScore
				then
					subScore1
				else
					bestScore
		in			
			if bestScoreTmp < alpha
			then
				((0, alpha, (positions+1), cutoffs),False)
			else
				let
					((subMove2, subScore2, subPosition2, subCutoffs2),stop) = inserimentoMigliore depth alpha bestScoreTmp positions cutoffs bestMove (tail listaPapabili)
				in
					if stop == False
					then
						((subMove2, subScore2, subPosition2, subCutoffs2),stop)
					else
						if subScore2>bestScoreTmp
						then
							((subMove1, subScore1, subPosition1, subCutoffs1),True)
						else
							((subMove2, subScore2, subPosition2, subCutoffs2),True)
	else
		((bestMove, bestScore ,positions ,cutoffs),True)


mossaComputer :: Int -> Float -> Float -> Int -> Int -> Board -> Int -> (Int, Float,Int,Int)
mossaComputer depth alpha bestScore positions cutoffs board bestMove =
	let
		numInserimentiPossibili=2
		elementiOttimi = valoriOttimi board numInserimentiPossibili
		(valoriInseriti,stop)=inserimentoMigliore depth alpha bestScore positions cutoffs bestMove elementiOttimi
	in
		valoriInseriti


--FINE CALCOLO MATTONELLA DA INSERIRE DA PARTE DEL COMPUTER

-- INIZIO FUNZIONI DI SUPPORTO

-- Restituisce True se all'interno della Board b è presente il valore x
cercaVal :: Int -> Board -> Bool
cercaVal x b = foldl (||) False . map (not . null . filter (== x)) $ b
	
-- Restituisce dato un intero tra 0 e 3 un vettore che indica una direzione di spostamento all'interno della board
vettoreDirezione :: Int -> (Int,Int)
vettoreDirezione direzione =
	if direzione == 0
	then
		(0,-1)
	else
		if direzione == 1
		then
			(1,0)
		else
			if direzione == 2
			then
				(0,1)
			else
				if direzione == 3
				then
					(-1,0)
				else
					let
						errore = qtrace $ "Impossibile chiamare la funzione con questo valore"
					in
						(0,0)

-- Stampa i valori indicati
qtrace :: Show a => a -> a
qtrace x = trace (show x) x

-- Torna il massimo valore della board
maxValueInt :: Board -> Int
maxValueInt = maximum . map maximum

-- Torna il massimo valore dell'indice dell'esponenziale base due che calcolato da il valore nella board
maxValue :: Board -> Int
maxValue board=
	let
		val = maxValueInt board
		val1 = log (fromIntegral (val) :: Float)/log 2
	in
		truncate(val1)


freeCellsInRow :: Int -> Row -> [Int]
freeCellsInRow _ []     = []
freeCellsInRow j (x:tl) = (if x==0 then [j] else []) ++ freeCellsInRow (j+1) tl
freeCells :: Int -> Board -> [(Int, Int)]
freeCells _ []     = []
freeCells i (r:tl) = map (\j -> (i, j)) (freeCellsInRow 0 r) ++ freeCells (i+1) tl

-- Data una board ritorna la lista delle celle libere
freeCell :: Board -> [(Int, Int)]
freeCell = freeCells 0

-- Inserisce un valore nella board nella cella indicata
addNumRow :: Int -> a -> [a] -> [a]
addNumRow 0  num (x:tl) = num:tl
addNumRow j  num (x:tl) = x : addNumRow (j-1) num tl
inserisciElemento :: (Int, Int) -> a -> [[a]] -> [[a]]
inserisciElemento (0,j) num (r:tl) = (addNumRow j num r) : tl
inserisciElemento (i,j) num (r:tl) = r : inserisciElemento (i-1, j) num tl

-- Restituisce un bool che indica se la cella è libera, se la cella non esiste restituisce che è libera
isFree :: Board -> (Int, Int) -> Bool
isFree b (x, y) = valoreCella b (x, y) == 0 || valoreCella b (x, y) == (-1)

-- Data una cella dice se è occupata
isNotFree :: Board -> (Int, Int) -> Bool
isNotFree b (x, y) = not (isFree b (x, y))

-- Restituisce il valore della cella specificata
valoreCella :: Board -> (Int, Int) -> Int
valoreCella b (x, y) =
	if (x>=0 && x<=3 && y>=0 && y<=3)
	then
		b !! x !! y
	else
		(-1)

-- FINE FUNZIONI DI SUPPORTO


--INIZIO MONOTONICITY

whileDueY :: Board -> Int -> Int -> Int
whileDueY board next y =
	if next<4 && (isFree board (next,y))
	then
		whileDueY board (next+1) y
	else
		next

whileIntLeftRight :: Board -> (Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
whileIntLeftRight board totals y x current next =
	let
		subNext = whileDueY board next y
		newNext =
			if subNext>=4
			then
				(subNext-1)
			else
				subNext
		currentValue =
			if (isNotFree board (current,y))
			then
				log (fromIntegral (valoreCella board (current,y)) :: Float)/log 2
			else
				0
		nextValue =
			if (isNotFree board (newNext,y))
			then
				log (fromIntegral (valoreCella board (newNext,y)) :: Float)/log 2
			else
				0
		(val0, val1, val2, val3) = totals
		newVal2=
			if currentValue > nextValue
			then
				val2 + (truncate(nextValue) - truncate(currentValue))
			else
				val2
		newVal3=
			if currentValue < nextValue
			then
				val3 + (truncate(currentValue) - truncate(nextValue))
			else
				val3
	in
		if next<4
		then
			whileIntLeftRight board (val0, val1, newVal2, newVal3) y x newNext (newNext+1)
		else
			(val0, val1, val2, val3)



leftRight :: Board -> (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
leftRight board totals y =
	let
		newTotals = whileIntLeftRight board totals y 0 0 1
	in
		if y<3
		then
			leftRight board newTotals (y+1)
		else
			newTotals



whileDueX :: Board -> Int -> Int -> Int
whileDueX board x next =
	if next<4 && (isFree board (x,next) )
	then
		whileDueX board x (next+1)
	else
		next


whileIntUpDown :: Board -> (Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
whileIntUpDown board totals x y current next =
	let
		subNext = whileDueX board x next
		newNext =
			if subNext>=4
			then
				(subNext-1)
			else
				subNext
		currentValue =
			if isNotFree board (x,current)
			then
				log (fromIntegral (valoreCella board (x,current)) :: Float)/log 2
			else
				0
		nextValue =
			if isNotFree board (x,newNext)
			then
				log (fromIntegral (valoreCella board (x,newNext)) :: Float)/log 2
			else
				0
		(val0, val1, val2, val3) = totals
		newVal0=
			if (currentValue > nextValue)
			then
				val0 + truncate(nextValue) - truncate(currentValue)
			else
				val0
		newVal1=
			if currentValue < nextValue
			then
				val1 + (truncate(currentValue) - truncate(nextValue))
			else
				val1
	in
		if next<4
		then
			whileIntUpDown board (newVal0, newVal1, val2, val3) x y newNext (newNext+1)
		else
			(val0, val1, val2, val3)

upDown :: Board -> (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
upDown board totals x =
	let
		newTotals = whileIntUpDown board totals x 0 0 1
	in
		if x<3
		then
			upDown board newTotals (x+1)
		else
			newTotals


-- Restituisce la monotonicity legata alla board passata come parametro
monotonicity2 :: Board -> Int
monotonicity2 board =
	let
		subTotals = upDown board (0,0,0,0) 0
		(val1, val2, val3, val4) = leftRight board subTotals 0
		max1=
			if val1>val2
			then
				val1
			else
				val2
		max2=
			if val3>val4
			then
				val3
			else
				val4
	in
		max1+max2

-- FINE MONOTONICITY

-- Restituisce il valore dell'euristica principale del gioco (l'evaluation appunto) relativa alla board passata
evaluation :: Board -> Float
evaluation board =
	let
		emptyCells = fromIntegral (length(freeCell board))
		smoothWeight = 0.1
		mono2Weight = 1.0
		emptyWeight = 2.7
		maxWeight = 1.0
	in
		((fromIntegral (smoothness board 0 0) :: Float) * smoothWeight) + ((fromIntegral (monotonicity2 board) :: Float) * mono2Weight) + ((log emptyCells) * emptyWeight) + ((fromIntegral(maxValue board)) * maxWeight)


-- INIZIO scelta mossa giocatore

scorriDirezioni :: Int -> Float -> Float -> Int -> Int -> Int -> Board -> Int -> (Int, Float,Int,Int)
scorriDirezioni depth bestScore beta positions cutoffs direction board bestMove =
	let
		(finalBestMove, finalBestScore, finalPositions, finalCutoffs, finalDepth, stop) =
			-- Valuta se la direzione può modificare la board, se si cerca di estendere l'albero verso la nuova configurazione, calcolando la mossa da compiere
			if (pushCk (toBoardT board) (toDirection direction)) /= Nothing
			then
				let
					boardIntermedia = push (toBoardT board) (toDirection direction)
					(newBestMove, newScore, newPositions, newCutoffs, newDepth, stop)=
						if gameEnded boardIntermedia == True
						then
							(direction, bestScore, (positions + 1), cutoffs, depth, True)
						else 
							let
								(subMove, subScore, newPositionsTmp, newCutoffsTmp) =
									if depth==0
									then
										(direction, evaluation (toBoard boardIntermedia), positions + 1, cutoffs)
									else
										let
											(subMove, subScoreTmp, subPositions, subCutoffs) = search (depth-1) bestScore beta (positions+1) cutoffs False (toBoard boardIntermedia)
											subScore=
												if subScoreTmp>9900
												then
													subScoreTmp-1
												else
													subScoreTmp
										in
											(subMove, subScore, subPositions, subCutoffs)
								(newBestScoreTmp, newBestMoveTmp) =
									if subScore>bestScore
									then
										(subScore, direction)
									else
										(bestScore,bestMove)
							in
								if newBestScoreTmp > beta
								then
									(newBestMoveTmp, beta, newPositionsTmp, (newCutoffsTmp+1), depth, True)
								else
									(newBestMoveTmp, newBestScoreTmp, newPositionsTmp, newCutoffsTmp, depth, False)
				in
					(newBestMove, newScore, newPositions, newCutoffs, newDepth, stop)		
			else
				(bestMove, bestScore, positions, cutoffs, depth, False)
	in
		if (stop==True || direction==3)
		then
			(finalBestMove, finalBestScore, finalPositions, finalCutoffs)
		else
			scorriDirezioni finalDepth finalBestScore beta finalPositions finalCutoffs (direction+1) board finalBestMove

-- FINE scelta mossa giocatore


-- Restituisce la mossa finale da compiere. In pratica la miglior mossa ritornata dall'albero minimax fin dove lo si è calcolato
search :: Int -> Float -> Float -> Int -> Int -> Bool -> Board -> (Int, Float ,Int ,Int)
search depth alpha beta positions cutoffs turn board
	| turn = scorriDirezioni depth alpha beta positions cutoffs 0 board (-1)
	| otherwise = mossaComputer depth alpha beta positions cutoffs board 1


-- Invoca la ricerca con i parametri corretti
passaggio :: BoardT -> Int -> Direction
passaggio b depthLimit=
	let
		(val1, val2, val3, val4) = search depthLimit (-10000) 10000 0 0 True (toBoard b)
	in
		if val1/=(-1)
		then
			toDirection val1
		else
			if (pushCk b (toDirection 0)) /= Nothing
			then
				toDirection 0
			else
				if (pushCk b (toDirection 1)) /= Nothing
				then
					toDirection 1
				else
					if (pushCk b (toDirection 2)) /= Nothing
					then
						toDirection 2
					else
						toDirection 3

type FunState = (Int)
fChoice :: Int -> FunState -> BoardT -> IO (Direction, FunState)
fChoice depthLimit move b = do
	let bestDir = passaggio b depthLimit
	putStrLn ("Move: " ++ show move ++ " Choice: " ++ show bestDir)
	return (bestDir, (move+1))


solver :: (Int) -> IO ()
solver maxDepth = do
	game 0 (fChoice maxDepth)
