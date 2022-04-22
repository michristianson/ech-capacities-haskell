{-# LANGUAGE DeriveGeneric, BangPatterns #-}
module Internals where

import Data.List
import Data.Monoid
import Data.Serialize
import Data.Maybe (fromMaybe, isNothing)
import Data.Function (on)
import Control.DeepSeq
import GHC.Generics (Generic)

import Text.ParserCombinators.Parsec

--Data Definitions, etc. {{{
-- |A Vertex is meant to represent a lattice point. These will be, for instance, vertices
-- of our convex integral paths as well as the points that we count up in order to compute
-- L for a convex generator. A Vertex is made by typing a 2-tuple of integers: for instance,
--
-- > let vert = (4,5)
type Vertex = (Int, Int)
-- |An Edge represents the displacement vector corresponding to an edge of a convex integral
-- path. Being a 2-tuple of integers, an Edge is syntactically identical to a Vertex; Haskell
-- cannot tell the difference between the two. Their names are purely for readability of function
-- names and of code.
type Edge = (Int, Int)
-- |A Decomposition represents all the data that is produced from applying Theorem 1.19.
-- Factorizations are represented as lists of convex generators (where each element of the
-- list is a factor); decompositions, therefore, are 2-tuples of factorizations. The first element
-- in the tuple represents the factorization of lambda' (i.e. the convex generator that we
-- apply Theorem 1.19 to); the second element represents the factorization of lambda.
type Decomposition = ([CGen], [CGen])
-- |Labels represent the letters attached to edges of a convex generator. As such, there are two
-- types of Labels: E and H. Note that these are not Strings but data types in their own right;
-- we don't type \"E\" but instead simply E. Also, due to Haskell's naming conventions, the letters
-- must be capitalized.
data Label = E | H deriving (Eq, Show, Read, Generic)
-- |CIP stands for "convex integral path." The data that a CIP holds is simply a list of Vertices,
-- which correspond to every lattice point along the convex integral path (NOT just the ones between
-- two edges of different slopes). That said, you cannot substitute a list of Vertices for a CIP;
-- they are two different data structures.
--
-- The function getVertices takes a CIP and returns the list of vertices that is stored in it:
-- for instance,
--
-- >>> getVertices (pathFromVerts [(0,2),(2,2),(2,0)])
-- [(0,2),(1,2),(2,2),(2,1),(2,0)]
--
-- (See below for information about how 'pathFromVerts' works.)
data CIP = CIP { getVertices :: [Vertex] } deriving (Eq, Show, Generic)
-- |A CGen represents a convex generator. It contains a convex integral path and a list of labels.
-- The Labels correspond to the labels on the edges of the convex generator, from left to right.
--
-- The functions getPath and getLabels both take a CGen as an argument and return the CIP and the
-- list of Labels, respectively, that are held in the CGen.
data CGen = CGen { getPath :: CIP, getLabels :: [Label] } deriving (Eq, Generic)
-- |CTD stands for "convex toric domain." Since convex toric domains are precisely the areas in the
-- first quadrant bounded by a CIP, the only data CTDs hold are the CIPs that make up their boundary.
-- However, for ease of use and internal efficiency, there are two more specific types of CTDs.
-- For any floats (i.e. deecimal numbers) a and b, "Ellipsoid a b" represents the ellipsoid
-- E(a,b), and "Polydisk a b" represents the polydisk P(a,b).
--
-- Note: the following syntax is required to make a CTD that is not an Ellipsoid or a Polydisk:
--
-- > let myctd = CTD { getBoundary = <boundary> }
--
-- where \<boundary\> is a CIP. Moreover, getBoundary is a function that take a CTD and returns
-- the CIP that is its boundary. However, getBoundary only works on generic CTDs, NOT on 
-- Ellipsoids or Polydisks.
data CTD = Ellipsoid Double Double | Polydisk Double Double | CTD { getBoundary :: CIP } deriving (Eq, Show)

--Ord instance for Label using lexicographical order (no longer used)
instance Ord Label where
  compare l1 l2 = (show l1) `compare` (show l2)

instance Ord CGen where
  compare = compare `on` (getVertices . getPath)

instance NFData Label
instance NFData CIP
instance NFData CGen
instance Serialize Label
instance Serialize CIP
instance Serialize CGen

--Implementation of serialization using the binary package instead of the cereal package. My
--tests indicate that both serialize to the same number of bytes. The Hackage page for cereal
--claims similar performance to binary, but the use of strict ByteStrings in cereal is better for
--our purposes because strict ByteStrings are necessary for HDBC's database storage.
--instance Binary Label where
--  put E = B.put (0 :: Word8)
--  put H = B.put (1 :: Word8)
--
--  get = do t <- B.get :: B.Get Word8
--           case t of
--              0 -> return E
--              1 -> return H
--
--instance Binary CIP where
--  put path = B.put (getVertices path)
--
--  get = do p <- B.get :: B.Get [(Int,Int)]
--           return (CIP p)
--
--instance Binary CGen where
--  put (CGen path labels) = do B.put path
--                              B.put labels
--
--  get = do path <- B.get
--           labels <- B.get
--           return (CGen path labels)

-- | Here we see that CGen is a Monoid. This means precisely what it would mathematically.
-- The function representing the monoid operation is called <> or mappend. As you might expect,
-- it takes 2 CGens as arguments and returns the CGen formed by concatenating the edges of the
-- input CGens. Since we usually write such a binary operation infixed (like we would with
-- + or *), it is standard to call mappend in this format as well: for example,
--
-- cg1 `mappend` cg2
--
-- Or, equivalently,
--
-- cg1 <> cg2
--
-- Note that mappend is surrounded with backticks (the thing on the tilde key), NOT
-- single quotation marks.
instance Semigroup CGen where
  cg <> cg' = let edges = getAllEdgeLabels cg
                  edges' = getAllEdgeLabels cg'
                  (es,ls) = unzip . combineEdgeLabels $ sortBy 
                                    (\(e1,l1) (e2,l2) -> edgeSlope e2 `compare` edgeSlope e1) (edges ++ edges')
              in CGen { getPath = _pathFromEdges es, getLabels = ls }
instance Monoid CGen where
  mempty = CGen { getPath = CIP { getVertices = [] }, getLabels = [] }
instance Show CGen where
  show cg = let edges = getEdgeFactors' . getPath $ cg
                labels = getLabels cg
                edgestring (edge,mult) label = (if mult == 1 then "" else show mult) ++ show label ++ show edge
            in foldl (\acc i -> acc ++ edgestring (edges !! i) (labels !! i)) "" [0..(length edges - 1)]

parseEdge :: Parser (Edge,Label)
parseEdge = do { mult <- many1 digit <|> return "1";
                 label <- letter;
                 char '(';
                 ex <- many1 digit;
                 char ',';
                 char '-' <|> return ' ';
                 ey <- many1 digit;
                 char ')';
                 return ((read ex * read mult,-read ey * read mult),read [label])
              }

parseCGen :: Parser CGen
parseCGen = do { edges <- many1 parseEdge;
                 return (let (es, ls) = unzip edges in makeCGen (pathFromEdges es) ls) }
                 
parseCGenWithRemaining :: Parser (CGen,String)
parseCGenWithRemaining = (,) <$> parseCGen <*> getInput

instance Read CGen where
  readsPrec _ str = case parse parseCGenWithRemaining "" str of Left err -> error (show err)
                                                                Right result -> [result]

-- This function is simply an application of read to the case of a CGen, made for
-- accessibility and backwards compatibility.
readCGen :: String -> CGen
readCGen str = read str :: CGen
--}}}
--Useful Helper Functions {{{
--Functions on lists
isAscending :: (Ord a) => [a] -> Bool
isAscending xs = and $ zipWith (<=) xs (tail xs)

isDescending :: (Ord a) => [a] -> Bool
isDescending xs = and $ zipWith (>=) xs (tail xs)

makeAllLists :: [[a]] -> [[a]]
makeAllLists xs = foldr (\x acc -> concatMap (\es -> map (:es) x) acc) [[]] xs

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

allMinimumsBy :: (a -> a -> Ordering) -> [a] -> [a]
allMinimumsBy _ [] = []
allMinimumsBy comp (x:xs) = _allMinsBy comp xs [x]

_allMinsBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
_allMinsBy _ [] sofar = sofar
_allMinsBy comp (x:xs) sofar
    | comp (head sofar) x == GT = _allMinsBy comp xs [x]
    | comp (head sofar) x == EQ = _allMinsBy comp xs (x:sofar)
    | comp (head sofar) x == LT = _allMinsBy comp xs sofar

--Slope functions
vertSlope :: Vertex -> Vertex -> Double 
vertSlope (!x1,!y1) (!x2,!y2) = fromIntegral (y2-y1) / fromIntegral (x2-x1)

vertSlopes :: [Vertex] -> [Double]
vertSlopes verts = zipWith vertSlope verts (tail verts)

edgeSlope :: Edge -> Double
edgeSlope (x,y) = fromIntegral y / fromIntegral x

edgeSlopes :: [Edge] -> [Double]
edgeSlopes = map edgeSlope

--Functions on edges and vertices
edgeBetween :: Vertex -> Vertex -> Edge
edgeBetween (x1,y1) (x2,y2) = (x2-x1,y2-y1)

traverseEdge :: Vertex -> Edge -> Vertex
traverseEdge (x,y) (dx,dy) = (x+dx,y+dy)

--Interpolation
interpolateVerts :: Vertex -> Vertex -> [Vertex]
interpolateVerts v1@(x1,y1) v2 = let (x,y) = edgeBetween v1 v2
                                     mult = gcd x y
                                     (dx,dy) =  (x `div` mult,y `div` mult)
                                 in map (\i -> (x1+i*dx,y1+i*dy)) [1..mult]

interpolateVerts' :: Vertex -> Vertex -> [Vertex]
interpolateVerts' v1@(x1,y1) v2 = let (x,y) = edgeBetween v1 v2
                                      mult = gcd x y
                                      (dx,dy) =  (x `div` mult,y `div` mult)
                                 in map (\i -> (x1+i*dx,y1+i*dy)) [0..(mult-1)]

interpolatePath :: [Vertex] -> [Vertex]
interpolatePath verts = let pairs = zip verts (tail verts)
                        in head verts : concatMap (uncurry interpolateVerts) pairs

interpolatePath' :: [Vertex] -> [Vertex]
interpolatePath' verts = let pairs = zip verts (tail verts)
                         in foldl (\acc (v1,v2) -> interpolateVerts' v2 v1 ++ acc) [head verts] pairs

interpolateLattice :: Vertex -> Vertex -> [Vertex]
interpolateLattice v1@(x1,y1) v2@(x2,y2) = let slope = vertSlope v1 v2
                                           in map (\i -> (x1+i,floor (fromIntegral y1 + fromIntegral i * slope)))
                                                  [1..(x2-x1)]

interpolateLattice' :: Vertex -> Vertex -> [Vertex]
interpolateLattice' v1@(x1,y1) v2@(x2,y2) = let slope = vertSlope v1 v2
                                            in map (\i -> (x1+i,floor (fromIntegral y1 + fromIntegral i * slope)))
                                                   [0..(x2-x1-1)]

--Functions on CIPs
getAllEdges :: CIP -> [Edge]
getAllEdges path = zipWith edgeBetween verts (tail verts)
                   where verts = getVertices path

getEdges :: CIP -> [Edge]
getEdges path = map (foldl traverseEdge (0,0)) (group edges)
                where edges = getAllEdges path

numEdgeFactors :: CIP -> Int
numEdgeFactors path = length (getVertices path) - 1

getEdgeFactors :: CIP -> [Edge]
getEdgeFactors path = map head (group edges)
                      where edges = getAllEdges path

getEdgeFactors' :: CIP -> [(Edge,Int)]
getEdgeFactors' path = map (\es -> (head es, length es)) (group edges)
                       where edges = getAllEdges path

--(Edge,Label) Functions
getAllEdgeLabels :: CGen -> [(Edge,Label)]
getAllEdgeLabels cg = let edges = group . getAllEdges . getPath $ cg
                      in concat $ zipWith (\es l -> if l == E then zip es (repeat E)
                                                    else zip es (replicate (length es - 1) E ++ [H]))
                                          edges (getLabels cg)

getEdgeLabels :: CGen -> [(Edge,Label)]
getEdgeLabels cg = zip (getEdges . getPath $ cg) (getLabels cg)

getEdgeLabelFactors :: CGen -> [(Edge,Label)]
getEdgeLabelFactors cg = zip (getEdgeFactors . getPath $ cg) (getLabels cg)

getEdgeLabelFactors' :: CGen -> [((Edge,Label),Int)]
getEdgeLabelFactors' cg = let edges = group . getAllEdges . getPath $ cg
                          in zipWith (\es l -> ((head es,l),length es)) edges (getLabels cg)
                                                        

combineEdgeLabels :: [(Edge,Label)] -> [(Edge,Label)]
combineEdgeLabels els = map (foldl (\acc el -> (traverseEdge (fst acc) (fst el),if snd acc == H
                                                                                then H else snd el))
                                   ((0,0),E)) (groupBy ((==) `on` fst) els)

--Miscellaneous
partitionInt :: Int -> [[Int]]
partitionInt 0 = [[]]
partitionInt i = [p:ps | p <- [1..i], ps <- _partitionInt (i-p) p]

_partitionInt :: Int -> Int -> [[Int]]
_partitionInt 0 _ = [[]]
_partitionInt i last = [p:ps | p <- [1..(min i last)], ps <- _partitionInt (i-p) p]

makeStraights :: Int -> (CGen,CGen)
makeStraights l = (horiz,vert)
                  where horiz = CGen { getPath = _pathFromEdges [(l,0)], getLabels = [E] }
                        vert = CGen { getPath = _pathFromEdges [(0,-l)], getLabels = [E] }

makeRectangle :: Int -> Int -> CGen
makeRectangle x y = CGen{ getPath = _pathFromVerts [(0,y),(x,y),(x,0)], getLabels = [E,E] }
--}}}
--Smart Constructors for CIP and CGen {{{
validVertices :: [Vertex] -> Bool
validVertices [] = False
validVertices verts = ((fst . head) verts == 0) && ((snd . head) verts >= 0) &&
                      ((snd . last) verts == 0) && ((fst . last) verts >= 0) &&
                      isAscending (map fst verts) && isDescending (0 : vertSlopes verts)

validEdges :: [Edge] -> Bool
validEdges edges = all (\(x,y) -> x >= 0 && y <= 0) edges && isDescending slopes
                   where slopes = edgeSlopes edges

validLabels :: CIP -> [Label] -> Bool
validLabels path labels = length edges == length labels &&
                          all (\i -> edgeSlope (edges !! i) /= 0 && edgeSlope (edges !! i) /= -1/0) hs
                          where edges = getEdgeFactors path
                                hs = filter (\i -> labels !! i == H) [0..(length labels - 1)]

-- |Given a list of vertices, this function will create the convex integral path formed by
-- connecting each of the vertices to its successor in the list with a straight line. Note
-- that the vertices must create a valid convex integral path: that is, the first vertex must be
-- on the postive y-axis, the last vertex must be on the positive x-axis, the x-values of the
-- vertices must be increasing, and convexity must be satisfied.
pathFromVerts :: [Vertex] -> CIP
pathFromVerts verts
    | validVertices verts = CIP { getVertices = interpolatePath verts }
    | otherwise = error "pathFromVerts: Invalid vertices"

-- |Given a list of edges, this function will create the convex integral path formed by
-- connecting each of these edges in the order they are given. Note that the edges must form a
-- valid convex integral path: that is, they must have non-negative x-values and non-positive
-- y-values, and the slopes must be descending.
pathFromEdges :: [Edge] -> CIP
pathFromEdges edges
    | validEdges edges = let y = foldl (\acc (x,y) -> acc - y) 0 edges
                             verts = scanl traverseEdge (0,y) edges
                         in CIP { getVertices = interpolatePath verts }
    | otherwise = error "pathFromEdges: Invalid edges"

-- |Given a CIP and a list of Labels, this function creates the convex generator formed by
-- labeling the first edge (from the left) of the CIP with the first label in the list,
-- the second edge with the second label, and so on. There must be precisely as many labels
-- as there are edges of the CIP, and recall that horizontal and vertical edges cannot be
-- labelled H.
makeCGen :: CIP -> [Label] -> CGen
makeCGen path labels
    | validLabels path labels = CGen { getPath = path, getLabels = labels }
    | otherwise = error "makeCGen: Invalid labels"

-- |A convenience function which takes a CIP and creates the convex generator formed by
-- labelling all the edges E.
makeAllEGen :: CIP -> CGen
makeAllEGen path = CGen { getPath = path, getLabels = replicate (length $ getEdges path) E }

--Unsafe constructors
_pathFromVerts :: [Vertex] -> CIP
_pathFromVerts verts = CIP { getVertices = interpolatePath verts }

_pathFromVerts' :: [Vertex] -> CIP
_pathFromVerts' verts = CIP { getVertices = interpolatePath' verts }

_pathFromEdges :: [Edge] -> CIP
_pathFromEdges edges = let y = foldl (\acc (x,y) -> acc - y) 0 edges
                           verts = scanl traverseEdge (0,y) edges
                       in CIP { getVertices = interpolatePath verts }
--}}}
--Basic Attributes of CGens {{{
-- |Computes x() for a given convex generator (i.e. the x-value of the final vertex of the generator).
getX :: (Num a) => CGen -> a
getX cg = fromIntegral . fst . last . getVertices . getPath $ cg

-- |Computes y() for a given convex generator (i.e. the y-value of the first vertex of the generator).
getY :: (Num a) => CGen -> a
getY cg = fromIntegral . snd . head . getVertices . getPath $ cg

-- |Computes h() for a given convex generator (i.e. the number of edges labelled H).
getH :: (Num a) => CGen -> a
getH cg = fromIntegral . length . filter (== H) . getLabels $ cg

-- |Computes m() for a given convex generator (i.e. the total multiplicity of all edges of
-- the generator, which is equal to the number of lattice points on the path of the generator minus one).
getM :: (Num a) => CGen -> a
getM cg = (fromIntegral . length . getVertices . getPath $ cg) - 1

-- |Computes L() for a given convex generator (i.e. the total number of lattice points enclosed
-- by the generator, including points on the generator and on the positive axes).
getL :: (Num a) => CGen -> a
getL cg = let verts = getVertices . getPath $ cg
              lattice = head verts : concatMap (uncurry interpolateLattice) (zip verts (tail verts))
          in fromIntegral $ foldl (\acc (x,y) -> acc + y + 1) 0 lattice

-- |Computes the index I() of the generator: for a generator cg,
--
-- > index cg = 2*(getL cg - 1) - getH cg
index :: (Num a) => CGen -> a
index cg = 2*(getL cg - 1) - getH cg
--}}}
--Actions of CGens{{{
tanPoint :: CIP -> Double -> Vertex
tanPoint path m = let edges = getEdges path
                      slopes = map edgeSlope edges
                      index = fromMaybe (length slopes) $ findIndex (\s -> m >= s) slopes
                  in foldl (\acc i -> traverseEdge acc (edges !! i)) (head $ getVertices path) [0..index-1]

det :: (Num a) => Edge -> Vertex -> a 
det (a,b) (c,d) = fromIntegral (a*d-b*c)

-- |Computes the symplectic action of a given convex generator with respect to a given convex toric domain.
-- Actions with respect to Ellipsoids and Polydisks are computed using the formulas specific to those convex
-- toric domains (see \"Beyond ECH Capacities\," Example 1.14); general domains are computed using the general
-- action formula (see \"Beyond ECH Capacities\," Definition 1.13).
action :: CTD -> CGen -> Double
action (Ellipsoid a b) cg = let (px,py) = tanPoint (getPath cg) (-b/a)
                            in b * fromIntegral px + a * fromIntegral py
action (Polydisk a b) cg = b * getX cg + a * getY cg
action ctd cg = let edges = getEdges . getPath $ cg
                in foldl (\acc e -> acc + det e (tanPoint (getBoundary ctd) (edgeSlope e))) 0 edges

--For a backwards [Vertex] instead of CGen. Useful for generateLessThan.
tanPointV :: [Vertex] -> Double -> Vertex
tanPointV verts m = let edges = map (uncurry edgeBetween) $ zip verts (tail verts)
                        slopes = map edgeSlope edges
                        index = fromMaybe (length slopes) $ findIndex (\s -> m <= s) slopes
                    in foldl (\acc i -> traverseEdge acc (edges !! i)) (last verts) [0..index-1]

actionV :: CTD -> [Vertex] -> Double
actionV (Ellipsoid a b) verts = let (px,py) = tanPointV verts (-b/a)
                                in b * fromIntegral px + a * fromIntegral py
actionV (Polydisk a b) verts = b*(fromIntegral . fst . head $ verts) + a*(fromIntegral . snd . last $ verts)
actionV ctd verts = let edges = zipWith edgeBetween verts (tail verts)
                    in foldl (\acc e -> acc + det e (tanPoint (getBoundary ctd) (edgeSlope e))) 0 edges
--}}}
--Finding Minimal CGens {{{
-- |This function computes a minimal CGen of a given index for a given CTD.
-- Note that by definition, minimal generators must have no edges labelled h;
-- so, if the submitted index is odd, no minimal generator is possible,
-- and the function will return an empty list.
generateMinimal :: CTD -> Int -> [CGen]
generateMinimal ctd i 
    | i `mod` 2 == 0 = let end = i `div` 2
                           (horiz,vert) = makeStraights end
                       in allMinimumsBy (compare `on` action ctd) $ 
                          horiz : vert : concatMap (\y -> minRect ctd y (i-2*y)) [1..(end - 1)]
    | otherwise = []

minRect :: CTD -> Int -> Int -> [CGen]
minRect ctd y i = let topx = fromIntegral i / (2*(fromIntegral y + 1))
                      topx' = floor topx
                      toprect = makeRectangle topx' y
                      norect = minTraps ctd [(0,y)] 0 i
                  in allMinimumsBy (compare `on` (action ctd)) $ if topx == fromIntegral topx'
                     then toprect : (norect ++ concatMap (\x -> minTraps ctd [(x,y),(0,y)] 0 (i-2*x*(y+1)))
                                                       [1..topx'-1])
                     else norect ++ concatMap (\x -> minTraps ctd [(x,y),(0,y)] 0 (i-2*x*(y+1))) [1..topx']

minTraps :: CTD -> [Vertex] -> Double -> Int -> [CGen]
minTraps ctd verts@((x1,y1):vs) lastm i = 
                      if i == 0 then [CGen (_pathFromVerts' ((x1,0):verts)) (replicate (length verts) E)] else
                         let leaf = minTriangles ctd verts lastm i
                         in allMinimumsBy (compare `on` action ctd) $
                            foldl (\acc y2 -> let y = y1-y2
                                                  new = concatMap snd . takeWhile ((>=) i . fst) $
                                                        map (\x -> let newi = (y1+y2+1)*x-y+gcd y x
                                                              in (newi,minTraps ctd ((x1+x,y2):verts) 
                                                                                 (edgeSlope (x,-y)) (i-newi)))
                                                            [1..if lastm == 0 then i `div` 2 
                                                                else ceiling (-fromIntegral y / lastm) - 1]
                                              in new ++ acc)
                                  leaf [1..(y1-1)]

minTriangles :: CTD -> [Vertex] -> Double -> Int -> [CGen]
minTriangles ctd verts@((x1,y1):vs) lastm i =
            take 1 $ foldl (\acc x -> let newi = (y1+1)*(x-1)+1+gcd y1 x
                                          newvs = (x1+x,0):verts
                                      in if newi == i
                                         then CGen (_pathFromVerts' newvs) (replicate (length newvs - 1) E) : acc
                                         else acc)
                           [] [1..if lastm == 0 then i `div` 2 else ceiling (-fromIntegral y1 / lastm) - 1]
--}}}
--Less Than and Theorem 1.19 Conditions {{{
-- |Given two CTDs, ctd and ctd', and two CGens, cg and cg', isLessThan ctd ctd' cg cg' returns
-- True if cg is less than or equal to cg' with respect to ctd and ctd' (see \"Beyond ECH Capacities\"
-- Definition 1.17) and False otherwise. Note that the first CTD supplied goes with the first 
-- CGen supplied, i.e. for the action condition, isLessThan ctd ctd' cg cg' will check that
--
-- > action ctd cg <= action ctd' cg'
isLessThan :: CTD -> CTD -> CGen -> CGen -> Bool
isLessThan ctd ctd' cg cg' = index cg == index cg' && action ctd cg <= action ctd' cg' &&
                             getX cg + getY cg - getH cg / 2 >= getX cg' + getY cg' + getM cg' - 1

-- |Given two CGens, this function returns True if the generators share no elliptical orbits
-- and False otherwise.
noSharedEOrbits :: CGen -> CGen -> Bool
noSharedEOrbits cg cg' = let edges = getEdgeLabelFactors cg
                             edges' = getEdgeLabelFactors cg'
                             shared = filter (`elem` edges') edges
                         in all (\e -> snd e /= E) shared

-- |Checks if a given Decomposition satisfies the second bullet point of Theorem 1.19: that is,
-- for any pair of indices i and j in the factorizations of the Decomposition, either
-- the factors at index i and j are equal in each of the factorizations, or the factors
-- at index i and j in the second factorization (correspondig to lambda, not lambda\') share no
-- elliptical orbit.
bullet2 :: Decomposition -> Bool
bullet2 (cgs', cgs) = let indices = makeAllLists [[0..(length cgs' - 1)], [0..(length cgs' - 1)]]
                      in all (\[i,j] -> (cgs' !! i == cgs' !! j && cgs !! i == cgs !! j) ||
                                        noSharedEOrbits (cgs !! i) (cgs !! j)) indices

-- |Checks if a given Decomposition satisfies the third bullet point of Theorem 1.19: that is,
-- for any subset of indices in the factorizations, if we take the product of all the factors at
-- those indices in the lambda factorization and the product of all the factors at those indices
-- in the lambda' factorization, the two resulting convex generators will have equal indices (I() values).
bullet3 :: Decomposition -> Bool
bullet3 (cgs', cgs) = all (\s -> length s == 1 || index (foldl (\acc i -> acc `mappend` (cgs !! i)) mempty s) ==
                                                  index (foldl (\acc i -> acc `mappend` (cgs' !! i)) mempty s))
                          (drop 1 $ powerSet [0..(length cgs' - 1)])

-- |Given two CTDs and a Decomposition, checks if the Decomposition satisfies all the conditions
-- of Theorem 1.19. This is equivalent to running the functions bullet2 and bullet3 on the Decomposition,
-- and then going through each of the factorizations and checking that the factors in the right
-- (lambda) factorizations are less than the corresponding factors in the left (lambda') factorization.
validDecomposition :: CTD -> CTD -> Decomposition -> Bool
validDecomposition ctd ctd' decomp@(cgs',cgs) = all (\i -> isLessThan ctd ctd' (cgs !! i) (cgs' !! i))
                                                    [0..(length cgs' - 1)] && bullet2 decomp && bullet3 decomp
--}}}
--Generating CGens with a Given I and all E's {{{
generateI :: Int -> [CGen]
generateI i
    | i `mod` 2 == 0 = let end = i `div` 2
                           (horiz,vert) = makeStraights end
                       in horiz : vert : concatMap (\y -> genRect y (i-2*y)) [1..(end - 1)]
    | otherwise = []

genRect :: Int -> Int -> [CGen]
genRect y i = let topx = fromIntegral i / (2*(fromIntegral y + 1))
                  topx' = floor topx
                  toprect = makeRectangle topx' y
                  norect = genTraps [(0,y)] 0 i
              in if topx == fromIntegral topx'
                 then toprect : (norect ++ concatMap (\x -> genTraps [(x,y),(0,y)] 0 (i-2*x*(y+1)))
                                                     [1..topx'-1])
                 else norect ++ concatMap (\x -> genTraps [(x,y),(0,y)] 0 (i-2*x*(y+1))) [1..topx']

genTraps :: [Vertex] -> Double -> Int -> [CGen]
genTraps verts@((x1,y1):vs) lastm i = 
                      if i == 0 then [CGen (_pathFromVerts' ((x1,0):verts)) (replicate (length verts) E)] else
                         let leaf = genTriangles verts lastm i
                         in foldl (\acc y2 -> let y = y1-y2
                                                  new = concatMap snd . takeWhile ((>=) i . fst) $
                                                        map (\x -> let newi = (y1+y2+1)*x-y+gcd y x
                                                              in (newi,genTraps ((x1+x,y2):verts) 
                                                                                 (edgeSlope (x,-y)) (i-newi)))
                                                            [1..if lastm == 0 then i `div` 2 
                                                                else ceiling (-fromIntegral y / lastm) - 1]
                                              in new ++ acc)
                                  leaf [1..(y1-1)]

genTriangles :: [Vertex] -> Double -> Int -> [CGen]
genTriangles verts@((x1,y1):vs) lastm i =
                     foldl (\acc x -> let newi = (y1+1)*(x-1)+1+gcd y1 x
                                          newvs = (x1+x,0):verts
                                      in if newi == i
                                         then CGen (_pathFromVerts' newvs) (replicate (length newvs - 1) E) : acc
                                         else acc)
                           [] [1..if lastm == 0 then i `div` 2 else ceiling (-fromIntegral y1 / lastm) - 1]
--}}}
