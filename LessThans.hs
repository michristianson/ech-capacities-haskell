module LessThans
(
  -- * Data Types
  Heuristics,
  -- * Heuristics Combinators
  defaultHeuristics,
  thm114Heuristics,
  useMaxY,
  useMaxYFn,
  noMaxY,
  -- * Generating Less Thans
  generateLessThan,
  --_generateLessThanB,
) where

import Internals
import IO

import qualified Data.Map.Lazy as Map
import qualified Math.Combinatorics.Multiset as MS

data Heuristics = Heuristics { maxYs :: (CGen,CTD) -> Int }

--Generating Less Thans {{{
-- This is the function that generates all CGens less than a given CGen.
-- The implementation of this process is given below; this is merely an accessor
-- function, so that we have a stable function name to refer to.
generateLessThan :: Heuristics -> CTD -> CTD -> CGen -> [CGen]
generateLessThan heuristics ctd ctd' cg' = makeShapes heuristics ctd ctd' cg'

-- This is similar to generateLessThan, but intended specifically for a
-- (still experimental) "bounding" feature in Obstruct.hs.
--_generateLessThanB :: Heuristics -> CTD -> CGen -> [CGen]
--_generateLessThanB heuristics ctd cg' = makeShapesB heuristics ctd cg'
--}}}
-- Heuristics Combinators {{{
--Note: this Heuristics is only valid under the following conditions:
--(1) We are searching for less thans for a CGen of the form dE(1,-1) for some d.
--(2) The CTD's for the search are ctd = Polydisk a 1 and ctd' = Ellipsoid c c
--    for some doubles a and c.
--(3) a >= 1 and c <= 2+a/2.
thm114Heuristics :: Heuristics
thm114Heuristics = Heuristics (\(cg,Polydisk a 1) ->
                                                 let d = (uncurry gcd) . head . getVertices . getPath $ cg
                                                 in floor ((fromIntegral d * (a-2)+2)/(2*(a-1))))

defaultHeuristics :: Heuristics
defaultHeuristics = Heuristics (const (-1))

useMaxY :: Int -> Heuristics -> Heuristics
useMaxY y (Heuristics f2)
    | y >= 0 = Heuristics (const y)
    | otherwise = Heuristics f2

useMaxYFn :: ((CGen,CTD) -> Int) -> Heuristics -> Heuristics
useMaxYFn fn _ = Heuristics fn

noMaxY :: Heuristics -> Heuristics
noMaxY _ = Heuristics (const (-1))
--}}}
--The "Make Shapes" Algorithm {{{
-- This is one of our generateLessThan algorithms. It works by building up CGens from left to right,
-- adding a rectangle, trapezoid, or triangle of any possibly valid size at each step.
-- TODO: Implement the genTable heuristic to find leaf nodes in addRect, addTraps, and addTriangles
makeShapes :: Heuristics -> CTD -> CTD -> CGen -> [CGen]
makeShapes heuristics ctd ctd' cg' = let i = index cg'
                                         acond = action ctd' cg'
                                         hcond = getX cg' + getY cg' + getM cg' - 1
                                         ybound = (maxYs heuristics) (cg',ctd)
                                     in addY heuristics ybound ctd i acond hcond
makeShapesB :: Heuristics -> CTD -> CGen -> [CGen]
makeShapesB heuristics ctd cg' = let i = index cg'
                                     hcond = getX cg' + getY cg' + getM cg' - 1
                                     ybound = (maxYs heuristics) (cg',ctd)
                                 in addY heuristics ybound ctd i (-1) hcond

addY :: Heuristics -> Int -> CTD -> Int -> Double -> Int -> [CGen]
addY heuristics ybound ctd i acond hcond
    | i `mod` 2 == 0 && i `div` 2 >= hcond = let end = i `div` 2
                                                 (horiz,vert) = makeStraights end
                                                 rest = concatMap (\y -> addRect heuristics ctd y (i-2*y)
                                                                                 acond (hcond-y))
                                                                  [1..if ybound >= 0
                                                                      then min ybound (end-1)
                                                                      else (end-1)]
                                             in if acond < 0 then horiz : vert : rest else
                                                if action ctd horiz <= acond then
                                                   if action ctd vert <= acond
                                                      then horiz : vert : rest
                                                      else horiz : rest
                                                else if action ctd vert <= acond
                                                        then vert : rest
                                                        else rest
    | otherwise = concatMap (\y -> addRect heuristics ctd y (i-2*y) acond (hcond-y))
                            [1..if ybound >= 0
                                then min ybound ((i `div` 2) - 1)
                                else ((i `div` 2) - 1)] 

addRect :: Heuristics -> CTD -> Int -> Int -> Double -> Int -> [CGen]
addRect heuristics ctd y i acond hcond = 
                        let topx = fromIntegral i / (2*(fromIntegral y + 1))
                            topx' = floor topx
                            toprect = makeRectangle topx' y
                            norect = addTrapsNR heuristics ctd [(0,y)] 0 i acond hcond
                        in if topx == fromIntegral topx' && topx' >= hcond &&
                              (acond < 0 || action ctd toprect <= acond)
                           then toprect : (norect ++ concatMap (\x -> addTrapsR heuristics ctd [(x,y),(0,y)] 0
                                                                                (i-2*x*(y+1)) acond (hcond-x))
                                                               [1..topx'-1])
                           else norect ++ concatMap (\x -> addTrapsR heuristics ctd [(x,y),(0,y)] 0 (i-2*x*(y+1))
                                                                     acond (hcond-x))
                                                    [1..topx']

addTrapsR :: Heuristics -> CTD -> [Vertex] -> Double -> Int -> Double -> Int -> [CGen]
addTrapsR heuristics ctd verts@((x1,y1):vs) lastm i acond hcond =
                         let topi = i+length verts - 1
                             leaves = if i <= 0 && i `div` 2 >= hcond &&
                                         (acond < 0 || actionV ctd ((x1,0):verts) <= acond)
                                      then addLabelsHV (-i) (_pathFromVerts' ((x1,0):verts)) ++
                                           addTrianglesR heuristics ctd verts lastm (i,topi) acond hcond
                                      else addTrianglesR heuristics ctd verts lastm (i,topi) acond hcond
                         in foldl (\acc y2 -> let y = y1-y2
                                                  new = concatMap snd . takeWhile ((>=) topi . fst) $
                                                        map (\x -> let newi = (y1+y2+1)*x-y+gcd y x
                                                              in (newi,addTrapsR heuristics ctd ((x1+x,y2):verts)
                                                                                 (edgeSlope (x,-y)) (i-newi)
                                                                                 acond (hcond-x)))
                                                            [1..if lastm == 0 then i `div` 2 
                                                                else ceiling (-fromIntegral y / lastm) - 1]
                                              in new ++ acc)
                                  leaves [1..(y1-1)]

addTrianglesR :: Heuristics -> CTD -> [Vertex] -> Double -> (Int,Int) -> Double -> Int -> [CGen]
addTrianglesR heuristics ctd verts@((x1,y1):vs) lastm (lowi,topi) acond hcond = 
                     concatMap snd . takeWhile ((>=) topi . fst) $
                     map (\x -> let i = (y1+1)*(x-1)+1+gcd y1 x
                                    h = i - lowi
                                    newvs = (x1+x,0):verts
                                in if h >= 0 && (-h) `div` 2 >= hcond - x &&
                                      (acond < 0 || actionV ctd newvs <= acond)
                                   then (i,addLabelsH h (_pathFromVerts' newvs))
                                   else (i,[]))
                          [1..if lastm == 0 then topi `div` 2 else ceiling (-fromIntegral y1 / lastm) - 1]

addLabelsH :: Int -> CIP -> [CGen]
addLabelsH 0 path = [CGen { getPath = path, getLabels = replicate (length (getEdges path)) E }]
addLabelsH h path
    | h == length (getEdges path) - 1 = 
            [CGen { getPath = path, getLabels = E : replicate (length (getEdges path)-1) H }]
    | otherwise = let labels = MS.permutations . MS.fromCounts $ [(H,h),(E,length (getEdges path) - h - 1)]
                  in map (\ls -> CGen path (E : ls)) labels
addLabelsHV :: Int -> CIP -> [CGen]
addLabelsHV 0 path = [CGen { getPath = path, getLabels = replicate (length (getEdges path)) E }]
addLabelsHV h path
    | h == length (getEdges path) - 2 = 
            [CGen { getPath = path, getLabels = E : (replicate (length (getEdges path)-2) H ++ [E]) }]
    | otherwise = let labels = MS.permutations . MS.fromCounts $ [(H,h),(E,length (getEdges path) - h - 2)]
                  in map (\ls -> CGen { getPath = path, getLabels = E : (ls ++ [E]) }) labels

addTrapsNR :: Heuristics -> CTD -> [Vertex] -> Double -> Int -> Double -> Int -> [CGen]
addTrapsNR heuristics ctd verts@((x1,y1):vs) lastm i acond hcond =
                         let topi = i+length verts
                             leaves = if i <= 0 && i `div` 2 >= hcond &&
                                         (acond < 0 || actionV ctd ((x1,0):verts) <= acond)
                                      then addLabelsV (-i) (_pathFromVerts' ((x1,0):verts)) ++
                                           addTrianglesNR heuristics ctd verts lastm (i,topi) acond hcond
                                      else addTrianglesNR heuristics ctd verts lastm (i,topi) acond hcond
                         in foldl (\acc y2 -> let y = y1-y2
                                                  new = concatMap snd . takeWhile ((>=) topi . fst) $
                                                        map (\x -> let newi = (y1+y2+1)*x-y+gcd y x
                                                              in (newi,addTrapsNR heuristics ctd ((x1+x,y2):verts)
                                                                                 (edgeSlope (x,-y)) (i-newi)
                                                                                 acond (hcond-x)))
                                                            [1..if lastm == 0 then i `div` 2
                                                                else ceiling (-fromIntegral y / lastm) - 1]
                                                            
                                              in new ++ acc)
                                  leaves [1..(y1-1)]

addTrianglesNR :: Heuristics -> CTD -> [Vertex] -> Double -> (Int,Int) -> Double -> Int -> [CGen]
addTrianglesNR heuristics ctd verts@((x1,y1):vs) lastm (lowi,topi) acond hcond =
                     concatMap snd . takeWhile ((>=) topi . fst) $
                     map (\x -> let i = (y1+1)*(x-1)+1+gcd y1 x
                                    h = i - lowi
                                    newvs = (x1+x,0):verts
                                in if h >= 0 && (-h) `div` 2 >= hcond - x &&
                                      (acond < 0 || actionV ctd newvs <= acond)
                                   then (i,addLabels h (_pathFromVerts' newvs))
                                   else (i,[]))
                         [1..if lastm == 0 then topi `div` 2 else ceiling (-fromIntegral y1 / lastm) - 1]
                                            
addLabels :: Int -> CIP -> [CGen]
addLabels 0 path = [CGen { getPath = path, getLabels = replicate (length (getEdges path)) E }]
addLabels h path
    | h == length (getEdges path) = [CGen { getPath = path, getLabels = replicate (length (getEdges path)) H }]
    | otherwise = let labels = MS.permutations . MS.fromCounts $ [(H,h),(E,length (getEdges path) - h)]
                  in map (\ls -> CGen { getPath = path, getLabels = ls }) labels
addLabelsV :: Int -> CIP -> [CGen]
addLabelsV 0 path = [CGen { getPath = path, getLabels = replicate (length (getEdges path)) E }]
addLabelsV h path
    | h == length (getEdges path) - 1 = 
            [CGen { getPath = path, getLabels = replicate (length (getEdges path)-1) H ++ [E] }]
    | otherwise = let labels = MS.permutations . MS.fromCounts $ [(H,h),(E,length (getEdges path) - h - 1)]
                  in map (\ls -> CGen { getPath = path, getLabels = ls ++ [E] }) labels
--}}}
