module Nneonneo.Solver where

import Board
import GameBase
import Nneonneo.Heur
import Data.List (maximumBy, foldl')
import Control.Parallel.Strategies 
import System.Random
import Debug.Trace (trace)
import Control.DeepSeq (force)


-- Valuta un nodo max
emMax :: Int -> Float -> Int -> Float -> BoardT -> (Float, Int)
emMax depthLimit cprobLimit depth cprob board
	| depth >= depthLimit || cprob <= cprobLimit =
		(heur board, 1)
	| otherwise = compEmMax
	where
		compEmMax :: (Float, Int)
		compEmMax = 
			foldl' (\(best, tot) (val, count) -> (max best val, tot + count))
				(0.0, 0) .
			map (emChance depthLimit cprobLimit depth cprob) .
			filter (/= board) .
			map (push board) $
			directions


-- Valuta un nodo chance
emChance :: Int -> Float -> Int -> Float -> BoardT -> (Float, Int)
emChance depthLimit cprobLimit depth cprob board
	| full = emMax depthLimit cprobLimit (depth+1) cprob board
	| otherwise = res
	where
		b2 = boardsWithRandomTileInserted 2 board
		b4 = boardsWithRandomTileInserted 4 board
		np = (fromIntegral $ length b2) :: Float
		full = length b2 == 0

		calcAvg :: Float -> ([BoardT] -> (Float, Int))
		calcAvg prob =
			(\(sum, tot) -> (sum * prob, tot)) .
			foldl' (\(sum, tot) (val, count) -> (sum + val, tot + count))
				(0.0, 0) .
			map (emMax depthLimit cprobLimit (depth+1) (cprob * prob))

		(b2s, b2c) = calcAvg (0.9/np) b2
		(b4s, b4c) = calcAvg (0.1/np) b4
		res = (b2s + b4s, b2c + b4c)


-- Valuta il nodo radice
emFirstChoice :: Int -> Float -> BoardT -> [(Direction, (Float, Int))]
emFirstChoice depthLimit cprobLimit board = 
	parMap rdeepseq (\(d, b) -> (d, emChance depthLimit cprobLimit 0 1 b)) .
	filter (\(d, b) -> b /= board) .
	map (\d -> (d, push board d)) $
	directions


type FunState = (Int, Int)

-- Funzione interrogata ad ogni passo da game
fChoice :: Int -> Float -> FunState -> BoardT -> IO (Direction, FunState)
fChoice depthLimit cprobLimit (move, evals) b = do
	let	chc = emFirstChoice depthLimit cprobLimit b
		nevals = sum . map snd . map snd $ chc
		tevals = evals + nevals

	putStrLn . show . map (\(d, (v,_)) -> (d, v)) $ chc
	let bestDir = fst (maximumBy (\(_, (v1, _)) (_, (v2, _)) -> compare v1 v2) chc)
	putStrLn ("Move: " ++ show move ++ " Evals: " ++ show nevals ++
		" Cumulative: " ++ show tevals ++ " Choice: " ++ show bestDir)
	return (bestDir, (move+1, tevals))


solver :: (Int, Float) -> IO ()
solver (maxDepth, minProb) = do
	game (0, 0) (fChoice maxDepth minProb)
























