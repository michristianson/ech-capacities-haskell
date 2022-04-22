module LessThans
(
  -- * Data Types
  Heuristics,
  -- * Heuristics Combinators
  defaultHeuristics,
  thm114Heuristics,
  memoizeLessThans,
  noMemoize,
  useMaxY,
  useMaxYFn,
  noMaxY,
  -- * Memoizer API
  initializeLTMem,
  prepareLTMem,
  prepareLTMem',
  destroyLTMem,
  -- * Generating Less Thans
  getLTs,
  generateLessThan,
  _generateLessThanB,
  with
) where

import Internals
import IO

import qualified Data.Map.Lazy as Map
import qualified Math.Combinatorics.Multiset as MS

import System.IO.Unsafe (unsafePerformIO)
import Database.HDBC (Statement)
import Database.HDBC.ODBC (Connection)

data LTMemoizer = None | WillMake | LTMem { dbConnPool :: (Connection,Statement,Statement),
                                            bigCache :: Maybe ((CTD,CTD,CGen),[CGen]),
                                            smallCache :: Map.Map (String,String,CGen) [CGen],
                                            maxEdges :: Int }
data Heuristics = Heuristics { memoizeLTs :: LTMemoizer, maxYs :: (CGen,CTD) -> Int, genTable :: String }

--LTMemoizers and getLTs {{{
--General usage of the API for the LTMemoizer object is as follows:
-- 1. Call initializeLTMem on a Heuristics object before search(es) to prepare the memoizer
--    for IO operations such as database querying;
-- 2. Call prepareLTMem for each search to specialize heuristics for the upcoming search;
-- 3. Perform a search, using getLTs with the Heuristics object to handle less-than generation; and
-- 4. call destroyLTMem to clear any IO settings (such as the database connection).
-- The current implementation demands that both database and in-memory cache be used
-- if any memoization is to occur. TODO: make this more flexible.

-- This function allows us to unwrap Heuristics into LTMemoizers using pattern matching and then
-- rewrap the LTMemoizer into an updated Heuristics object.
updateLTMemoizer :: Heuristics -> LTMemoizer -> Heuristics
updateLTMemoizer (Heuristics _ my gt) newmem = Heuristics newmem my gt

--API Functions
initializeLTMem :: Heuristics -> IO Heuristics
initializeLTMem heur = case memoizeLTs heur of
                            None -> return heur
                            WillMake -> do
                                            newconn <- makeLTDBConn
                                            let newmem = LTMem newconn Nothing Map.empty (-1)
                                            return $ updateLTMemoizer heur newmem
                            LTMem conn big small me -> do
                                                           destroyLTDBConn conn
                                                           newconn <- makeLTDBConn
                                                           let newmem = LTMem newconn big small me
                                                           return $ updateLTMemoizer heur newmem

prepareLTMem :: Heuristics -> CGen -> Heuristics
prepareLTMem heur maxcg =
             case memoizeLTs heur of
                  None -> heur
                  WillMake -> error "prepareLTMem called before initializeLTMem. Perhaps you forgot to use `with`?"
                  LTMem (conn,qst,sst) big small _ -> let me = numEdgeFactors (getPath maxcg) `div` 2
                                                          newmem = LTMem (conn,qst,sst) big small me
                                                      in updateLTMemoizer heur newmem

prepareLTMem' :: Heuristics -> CGen -> (Heuristics,Int)
prepareLTMem' heur maxcg =
             case memoizeLTs heur of
                  None -> (heur,numEdgeFactors (getPath maxcg))
                  WillMake -> error "prepareLTMem' called before initializeLTMem. Perhaps you forgot to use `with`?"
                  LTMem (conn,qst,sst) big small _ ->
                                                  let me = numEdgeFactors (getPath maxcg)
                                                      newmem = LTMem (conn,qst,sst) big small (me `div` 2)
                                                  in (updateLTMemoizer heur newmem,me)

destroyLTMem :: Heuristics -> IO Heuristics
destroyLTMem heur = case memoizeLTs heur of
                         None -> return $ updateLTMemoizer heur None
                         WillMake -> return $ updateLTMemoizer heur WillMake
                         LTMem (conn,qst,sst) _ _ _ -> do
                                                           destroyLTDBConn (conn,qst,sst)
                                                           return $ updateLTMemoizer heur WillMake

--Functions used by the getLTs function to update the less-than cache
lookupBig :: Maybe ((CTD,CTD,CGen),[CGen]) -> CTD -> CTD -> CGen -> Maybe [CGen]
lookupBig Nothing _ _ _ = Nothing
lookupBig (Just ((bctd,bctd',bcg'),lts)) ctd ctd' cg' = if bctd == ctd && bctd' == ctd' && bcg' == cg'
                                                        then Just lts
                                                        else Nothing

cacheLTs :: Heuristics -> CTD -> CTD -> CGen -> [CGen] -> Bool -> Heuristics
cacheLTs heuristics ctd ctd' cg' lts isbig =
           let newmem = cacheLTs' (memoizeLTs heuristics) ctd ctd' cg' lts isbig
           in updateLTMemoizer heuristics newmem

cacheLTs' :: LTMemoizer -> CTD -> CTD -> CGen -> [CGen] -> Bool -> LTMemoizer
cacheLTs' (LTMem (conn,qst,sst) _ small me) ctd ctd' cg' lts True =
           let newbig = Just ((ctd,ctd',cg'),lts)
           in LTMem (conn,qst,sst) newbig small me
cacheLTs' (LTMem (conn,qst,sst) big small me) ctd ctd' cg' lts False =
           let newsmall = Map.insert (show ctd,show ctd',cg') lts small
           in LTMem (conn,qst,sst) big newsmall me

-- This function uses the memoizeLTs value of the input Heuristics to obtain
-- generators less than a given generator possibly using memoization.
getLTs :: Heuristics -> CTD -> CTD -> CGen -> (Heuristics,[CGen])
getLTs heuristics ctd ctd' cg' =
        case memoizeLTs heuristics of
             None -> (heuristics,generateLessThan heuristics ctd ctd' cg')
             LTMem (conn,qst,sst) big small maxe ->
                           let isbig = numEdgeFactors (getPath cg') >= maxe
                               cachelookup = if isbig
                                             then lookupBig big ctd ctd' cg'
                                             else Map.lookup (show ctd,show ctd',cg') small
                           in case cachelookup of
                               Just res -> (heuristics,res)
                               Nothing -> case unsafePerformIO (queryLTDB qst ctd ctd' cg') of
                                               Just res -> let newheur = cacheLTs heuristics ctd ctd' cg'
                                                                                  res isbig
                                                           in (newheur,res)
                                               Nothing -> let lts = generateLessThan heuristics ctd ctd' cg'
                                                              newheur = cacheLTs heuristics ctd ctd' cg'
                                                                                 lts isbig
                                                          in unsafePerformIO (storeInLTDB (conn,sst) ctd ctd' cg' lts) `seq` (newheur,lts)

-- This is the function that generates all CGens less than a given CGen.
-- The implementation of this process is given below; this is merely an accessor
-- function, so that we have a stable function name to refer to.
generateLessThan :: Heuristics -> CTD -> CTD -> CGen -> [CGen]
generateLessThan heuristics ctd ctd' cg' = makeShapes heuristics ctd ctd' cg'

_generateLessThanB :: Heuristics -> CTD -> CGen -> [CGen]
_generateLessThanB heuristics ctd cg' = makeShapesB heuristics ctd cg'

-- This function uses currying to handle initializing and destroying the
-- LTMemoizer automatically. One must, however, ensure that the first argument of the
-- function calls prepareLTMem when it needs to.
infixl 0 `with`
with :: (Heuristics -> a) -> Heuristics -> a
with comp heur = let ltinit = unsafePerformIO $ initializeLTMem heur
                     res = comp ltinit
                 in (res `seq` unsafePerformIO (destroyLTMem ltinit)) `seq` res
--}}}
-- Heuristics Combinators {{{
--Note: this Heuristics is only valid under the following conditions:
--(1) We are searching for less thans for a CGen of the form dE(1,-1) for some d.
--(2) The CTD's for the search are ctd = Polydisk a 1 and ctd' = Ellipsoid c c
--    for some doubles a and c.
--(3) a >= 1 and c <= 2+a/2.
thm114Heuristics :: Heuristics
thm114Heuristics = Heuristics WillMake (\(cg,Polydisk a 1) ->
                                                 let d = (uncurry gcd) . head . getVertices . getPath $ cg
                                                 in floor ((fromIntegral d * (a-2)+2)/(2*(a-1))))
                              ""

defaultHeuristics :: Heuristics
defaultHeuristics = Heuristics None (const (-1)) ""

memoizeLessThans :: Heuristics -> Heuristics
memoizeLessThans (Heuristics _ f2 f3) = Heuristics WillMake f2 f3

noMemoize :: Heuristics -> Heuristics
noMemoize (Heuristics _ f2 f3) = Heuristics None f2 f3

useMaxY :: Int -> Heuristics -> Heuristics
useMaxY y (Heuristics f1 f2 f3)
    | y >= 0 = Heuristics f1 (const y) f3
    | otherwise = Heuristics f1 f2 f3

useMaxYFn :: ((CGen,CTD) -> Int) -> Heuristics -> Heuristics
useMaxYFn fn (Heuristics f1 _ f3) = Heuristics f1 fn f3

noMaxY :: Heuristics -> Heuristics
noMaxY (Heuristics f1 _ f3) = Heuristics f1 (const (-1)) f3

--TODO: make genTable function

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
