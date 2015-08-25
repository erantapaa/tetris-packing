{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}

module Lib
where

import Control.Monad
import Data.List (nubBy, inits, tails, foldl', sort, group)

import qualified Data.BitSet.Dynamic as Bits
import Data.BitSet.Dynamic (BitSet)

import Debug.Trace

data BitGrid = BitGrid { bg_cols :: Int, bg_bits :: BitSet Int }
  deriving (Show, Eq)

emptyGrid cols = BitGrid cols Bits.empty

-- allclear is True if none of the points are set in the grid. 
testPoints :: [(Int,Int)] -> BitGrid -> (Bool, BitGrid)
testPoints points grid = (allclear, theUnion)
  where
    bits = Bits.fromList [ r*(bg_cols grid) + c | (r,c) <- points ]
    theUnion = BitGrid (bg_cols grid) (Bits.union bits (bg_bits grid))
    allclear = Bits.null $ Bits.intersection bits (bg_bits grid)

data Piece = A | B | C | D | E | Blank
  deriving (Show, Eq, Enum, Bounded, Ord)

type Point = (Int,Int)

type Shape = [Point]

{-
 Pieces:
                 X      XX       XX        X
 A = XXXX, B = XXX, C = XX, D = XX  , E = XXX
-}

basicA = [ (0,0), (0,1), (0,2), (0,3) ]
basicB = [ (0,0), (0,1), (0,2), (1,2) ]
basicC = [ (0,0), (0,1), (1,0), (1,1) ]
basicD = [ (0,0), (0,1), (1,1), (1,2) ]
basicE = [ (0,0), (0,1), (1,1), (0,2) ]

translate :: Point -> Shape -> Shape
translate (dx,dy) points = [ (px-dx,py-dy) | (px,py) <- points ]

translations :: Shape -> [Shape]
translations points = [ translate p points | p <- points ]

-- Flip along the y-axis
mirror :: Shape -> Shape
mirror points = [ (-px,py) | (px,py) <- points ]

-- Rotate clockwise
rotate :: Shape -> Shape
rotate points = [ (py,-px) | (px,py) <- points ]

shapeToBits :: Shape -> BitGrid
shapeToBits points = grid
  where
    mx = minimum $ map fst points
    my = minimum $ map snd points
    points' = translate (mx,my) points
    (_, grid) = testPoints points' (emptyGrid 4)

uniqueShapes :: [Shape] -> [Shape]
uniqueShapes shapes = map fst $ nubBy (\(_,b1) (_,b2) -> b1 == b2) $ zip shapes (map shapeToBits shapes)

-- filter out shapes which extend up and to the left
removeUpLeft :: [Shape] -> [Shape]
removeUpLeft = filter go
  where go :: Shape -> Bool
        go points = all (\(dx,dy) -> dx >= 0 && dy >= 0) points

allOrientations :: Shape -> [Shape]
allOrientations shape = removeUpLeft $ concatMap translations $ uniqueShapes $ (rots ++ map mirror rots)
  where
    rots = take 4 $ iterate rotate shape

allAs = allOrientations basicA
allBs = allOrientations basicB
allCs = allOrientations basicC
allDs = allOrientations basicD
allEs = allOrientations basicE

orientations :: Piece -> [Shape]
orientations A = allAs
orientations B = allBs
orientations C = allCs
orientations D = allDs
orientations E = allEs
orientations Blank = [[(0,0)]]

type Placement = ( Piece, [ (Int,Int) ] )

data Solution = Solution Int [Placement]
 deriving (Show, Eq, Ord)

addPlacement :: Placement -> Solution -> Solution
addPlacement place@(p,_) (Solution n placements) = Solution n' (place:placements)
  where n' = if p == Blank then n else n+1

nPlacements :: Solution -> Int
nPlacements (Solution n _) = n

placements (Solution _ ps) = ps

nCells :: Solution -> Int
nCells (Solution _ ps) = sum $ map (length . snd) ps

type PieceCount = [ (Piece, Int) ]

-- Return all possible ways of using a piece.
pieceOptions :: PieceCount -> [ (Piece, PieceCount) ]
pieceOptions pieces = [ (p, as ++ [(p,c-1)] ++ bs) | (as, (p,c), bs) <- opts, c > 0 ]
  where opts = [  (as, b, bs) | (as, (b:bs)) <- zip (inits pieces) (tails pieces) ]

-- return the first available square; skip over the border
availableSquare :: Int -> BitGrid -> Maybe Point
availableSquare rows grid =
  let cols = bg_cols grid
      clear = [ (r,c) | r <- [1..rows],
                        c <- [1..cols-1], 
                        not $ Bits.member (r*cols+c) (bg_bits grid) ]
  in
  case clear of
    []        -> Nothing
    (rc : _ ) -> Just rc

-- number of blank cells left in grid
freeCells rows current grid
 = rows * ((bg_cols grid) - 2) - (sum $ map (pieceSize . fst) (placements current))
  where
    pieceSize Blank = 1
    pieceSize _     = 4

-- branch-and-bound search
search2 :: Solution    -- best solution so far
        -> Int         -- max rows
        -> Solution    -- current position
        -> BitGrid     -- occupied cells
        -> PieceCount  -- pieces remaining
        -> Solution
search2 best rows current grid pieces =
  -- check here if current can beat best
  let free = freeCells rows current grid
      maxPossible = nPlacements current + (free `div` 4)
      cell = availableSquare rows grid
      msg = "best: " ++ show n ++ " cell: " ++ show cell ++ " maxPoss: " ++ show maxPossible
        where n = nPlacements best
      canPrune = nPlacements best >= maxPossible
  in
  if canPrune    -- or: trace msg canPrune
    then best
    else case cell of
          Nothing -> best
          Just (r,c) -> let step sol (shape, p, pieces') =
                               let ps = [ (r+x,c+y) | (x,y) <- shape ]
                                   (allclear, grid') = testPoints ps grid in
                               if allclear
                                  then let current' = addPlacement (p, ps) current
                                           sol' = max sol current'
                                       in search2 sol' rows current' grid' pieces'
                                  else sol
                            children = [ (shape, p, pieces') | (p, pieces') <- pieceOptions pieces, shape <- orientations p ]
                        in foldl' step best children

-- depth-first search without any pruning
search1 :: Solution    -- best solution so far
        -> Int         -- max rows
        -> BitGrid     -- occupied cells
        -> PieceCount  -- pieces remaining
        -> Solution
search1 best rows grid pieces =
  case availableSquare rows grid of
    Nothing -> best
    Just (r,c) -> let children = do (p, pieces') <- pieceOptions pieces
                                    shape <- orientations p
                                    let ps = [ (r+x,c+y) | (x,y) <- shape ]
                                        (allclear, grid') = testPoints ps grid
                                    guard allclear
                                    let best' = addPlacement (p, ps) best
                                    return $ search1 best' rows grid' pieces'
                  in maximum $ [best] ++ children

solve1 :: (Int,Int,Int,PieceCount,BitGrid) -> Solution
solve1 (rows, cols, npieces, pieces, grid) =
  search1 sol0 rows grid pieces
  where sol0 = Solution 0 []

solve2 :: (Int,Int,Int,PieceCount,BitGrid) -> Solution
solve2 (rows, cols, npieces, pieces, grid) =
  search2 sol0 rows sol0 grid pieces
  where sol0 = Solution 0 []

runSolver solver str = do
  let setup@(rows, cols, npieces, pieces, grid) = readProblem str
      m = max ( (div rows 4) * (div cols 2) )
              ( (div cols 4) * (div rows 2) )
  if m >= npieces
    then putStrLn "all pieces can fit"
    else print $ solver setup

readPieceCount :: [String] -> PieceCount
readPieceCount ws =  map (\xs -> (head xs, length xs)) $ group $ sort [ x | Just x <- map go ws ]
  where
    go "A" = Just A
    go "B" = Just B
    go "C" = Just C
    go "D" = Just D
    go "E" = Just E
    go _   = Nothing

readProblem :: String -> (Int, Int, Int, PieceCount, BitGrid)
readProblem str =
  let (rs : cs : rest) = words str
      pieces = readPieceCount rest
      n = sum $ map snd pieces
      rows = read rs
      cols = read cs
      border = [ (0,c) | c <- [0..cols+1] ] ++ [ (rows+1,c) | c <- [0..cols+1] ]
                 ++ [ (r,0) | r <- [0..rows+1] ] ++ [ (r,cols+1) | r <- [0..rows+1] ]
      (_, grid) = testPoints border $ emptyGrid (cols+2)
      sol0 = Solution 0 []
      pieces' = pieces ++ [ (Blank, rows*cols) ]
  in (rows, cols, n, pieces', grid)

solveFile path solver = do
  contents <- readFile path
  runSolver solver contents

test1 = runSolver solve1 "2 4 C C"
test2 = runSolver solve1 "2 5 A A D D D"
test3 = runSolver solve1 "4 4 C C B E"

test4 = runSolver solve2 ("10 10" ++ concat (replicate 25 " C"))

