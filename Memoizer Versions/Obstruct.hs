{-# LANGUAGE BangPatterns #-}
module Obstruct
(
  -- * Re-exports
  module Internals,
  -- From LessThans
  Heuristics,
  defaultHeuristics,
  thm114Heuristics,
  memoizeLessThans,
  noMemoize,
  useMaxY,
  useMaxYFn,
  noMaxY,
  generateLessThan,
  with,
  -- *Data Types
  ScanDomain(..),
  -- * Obstructing Symplectic Embeddings
  factorizations,
  decompositions,
  obstruct,
  obstruct',
  thm119,
  thm119',
  thm114,
  thm114',
  -- * Scanning and Bounding
  normalScan,
  squeezeScan,
  -- * Scan Output (re-exported from IO)
  removeOffsets,
  scanToStd,
  scanToFile,
  scanToFileAndStd
) where

import Internals
import LessThans
import IO

import Data.Decimal
import Data.Monoid
import Data.List (foldl',find,partition)
import Data.Maybe (fromJust,isNothing)

import qualified Data.Map.Strict as Map
import Math.Combinat.Partitions.Multiset (partitionMultiset)

data ScanDomain = ScanDomain { getCTDs :: Double -> CTD, getCTDs' :: Double -> CTD, getCGs :: [CGen],
                               getARange :: (Double,Double,Double), getMidC  :: Double -> Double,
                               getCRange :: (Double,Double,Double) }

--Useful helper functions {{{
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 p (x:xs) | p x = x : takeWhile1 p xs
                    | otherwise = [x]

allPartition :: (b -> a -> Bool) -> [b] -> [a] -> ([a],[(a,b)])
allPartition _ [] xs = (xs,[])
allPartition cond (y:ys) xs = recurse fn' cond ys fp
                              where (fp,fn) = partition (cond y) xs
                                    fn' = map ((flip (,)) y) fn
                                    recurse nos _ [] as = (as,nos)
                                    recurse nos cond (b:bs) as = let (ps,ns) = partition (cond b) as
                                                                     ns' = map ((flip (,)) b) ns
                                                                 in recurse (nos++ns') cond bs ps

makeRange :: (RealFrac t,Read t) => t -> t -> t -> [t]
makeRange minf maxf step = let (mind,maxd,stepd) = (toDecimal minf,toDecimal maxf,toDecimal step)
                           in map fromDecimal $ takeWhile (maxd >=) [mind,mind+stepd..]

makeRange' :: (RealFrac t) => t -> t -> t -> [Decimal]
makeRange' minf maxf step = let (mind,maxd,stepd) = (toDecimal minf,toDecimal maxf,toDecimal step)
                           in takeWhile (maxd >=) [mind,mind+stepd..]

makeSubRange :: (RealFrac t,Read t) => [Decimal] -> (Decimal -> Decimal) -> t -> t -> t -> [[t]]
makeSubRange as midc loff roff cstep = let (ld,rd,stepd) = (toDecimal loff,toDecimal roff,toDecimal cstep)
                                       in map (\a -> let c = midc a in map fromDecimal $
                                                                       takeWhile (c+rd >=) [c+ld,c+ld+stepd..]) as

makeRange11 :: (RealFrac t1,Read t1,RealFrac t2,Read t2) => (t1,t1,t1) -> (t1 -> t2) -> (t2,t2,t2) ->
                                                                                        ([t1],[[t2]])
makeRange11 (mina,maxa,astep) midc (minc,maxc,cstep) = let as = makeRange' mina maxa astep
                                                           cs = makeSubRange as (toDecimal . midc . fromDecimal) 
                                                                             minc maxc cstep
                                                       in (map fromDecimal as,cs)
--}}}
--Factorizations, Decompositions, and Theorem 1.19 {{{
-- |Computes all factorizations of a given CGen.
factorizations :: CGen -> [[CGen]]
factorizations cg = let els = getAllEdgeLabels cg 
                        parts = partitionMultiset els
                    in map (map (\p -> let (es,ls) = unzip $ combineEdgeLabels p
                                       in CGen { getPath = _pathFromEdges (reverse es), getLabels = ls })) parts

-- Helper functions for decompositions and obstruct {{{
_decompUnder ctd ctd' (heur,me) facts = foldr (\(f,left,len) (h,acc) ->
                                               if len <= me
                                               then let (nh,res) = getLTs h ctd ctd' f
                                                        newans = map (\lt -> ((f,lt),left,len)) res
                                                    in (nh,newans++acc)
                                               else (h,acc))
                                     (heur,[]) facts

--These functions return a list of all factors of the given
--cgen, paired with a list of all unused edge-labels (with multiplicity).
getAllFactors :: CGen -> [(CGen,[((Edge,Label),Int)],Int)]
getAllFactors cg = getAllFactors' $ getEdgeLabelFactors' cg

getAllFactors' :: [((Edge,Label),Int)] -> [(CGen,[((Edge,Label),Int)],Int)]
getAllFactors' elms = map (\(esin,esout,len) -> (makeCG esin,esout,len)) $ _pickedges ([],[],0) elms
                      where makeCG els = let es = map (\(((x,y),_),m) -> (m*x,m*y)) els
                                             ls = map (snd . fst) els
                                         in CGen (_pathFromEdges es) ls

_pickedges :: ([((Edge,Label),Int)],[((Edge,Label),Int)],Int) -> [((Edge,Label),Int)] ->
              [ ( [((Edge,Label),Int)] , [((Edge,Label),Int)] , Int ) ] 
_pickedges (esin,esout,len) [] = if null esin then [] else [(esin,esout,len)]
_pickedges (esin,esout,len) left@((e,mult):es)
 | snd e == E = let pickall = _pickedges (esin ++ [(e,mult)],esout,len+mult) es
                    picknone = _pickedges (esin,esout ++ [(e,mult)],len) es
                    pickrest = concatMap (\i -> _pickedges (esin ++ [(e,i)],esout ++ [(e,mult-i)],len+i) es)
                                         [1..mult-1]
                    total = pickall ++ pickrest
                in if null esin then total else (esin,esout++left,len) : total
 | snd e == H = let pickall = _pickedges (esin ++ [(e,mult)],esout,len+mult) es
                    picknone = _pickedges (esin,esout ++ [(e,mult)],len) es
                    pickrest = concatMap (\i -> let withe = _pickedges (esin++[((fst e,E),i)],esout++[(e,mult-i)],
                                                                        len+i) es
                                                    withh = _pickedges (esin++[(e,i)],esout++[((fst e,E),mult-i)],
                                                                        len+i) es
                                                in withe ++ withh) [1..mult-1]
                    total = pickall ++ pickrest
                in if null esin then total else (esin,esout++left,len) : total
--}}}

-- |Computes all the Decompositions of a given CGen which satisfy the first bullet point of
-- Theorem 1.19.
decompositions :: CTD -> CTD -> CGen -> Heuristics -> [Decomposition]
decompositions ctd ctd' cg' heuristics = 
       snd $ foldl' (\(h,acc) ((f,lt),left,len) -> let (nh,ne) = recurse h ([f],[lt]) left len
                                                   in (nh,ne ++ acc))
              (heuristics',[]) withlts
       where factors = getAllFactors cg'
             (heuristics',withlts) = _decompUnder ctd ctd' (prepareLTMem' heuristics cg') factors
             recurse heur sofar [] _ = (heur,[sofar])
             recurse heur (fact',fact) left lastes = let fs = getAllFactors' left
                                                         (heur',lts) = _decompUnder ctd ctd' (heur,lastes) fs
                                                     in foldl' (\(h,acc) ((f',f),l,len) ->
                                                             let (nh,ne) = recurse h (f':fact',f:fact) l len
                                                             in (nh,ne ++ acc))
                                                                     (heur',[]) lts 

-- These functions uses the crition of Theorem 1.19 to attempt to obstruct a symplectic embedding.
-- The first returns a valid Decomposition if one exists (indicating a valid embedding may exist)
-- or else Nothing (indicating that no embedding can exist). The second returns a list of all
-- valid Decompositions, so that the return of an empty list indicates an obstruction to an
-- embedding.
obstruct :: CTD -> CTD -> CGen -> Heuristics -> Maybe Decomposition
obstruct ctd ctd' cg' heuristics =
       getFirst . snd $ foldl' (\(h,ans) ((f,lt),left,len) -> let (nh,ne) = recurse h ([f],[lt]) left len
                                                              in (nh,ans `mappend` ne))
                               (heuristics',First Nothing) withlts
       where factors = getAllFactors cg'
             (heuristics',withlts) = _decompUnder ctd ctd' (prepareLTMem' heuristics cg') factors
             recurse heur sofar [] _ = (heur,First (Just sofar))
             recurse heur (fact',fact) left lastes = let fs = getAllFactors' left
                                                         (heur',lts) = _decompUnder ctd ctd' (heur,lastes) fs
                                                     in foldl' (\(h,ans) ((f',f),l,len) ->
                                                             if validAdd (fact',fact) (f',f)
                                                             then let (nh,ne) = recurse h (f':fact',f:fact) l len
                                                                  in (nh,ans `mappend` ne)
                                                             else (h,ans))
                                                               (heur',First Nothing) lts 
             validAdd (d',d) (f',f) = all (\i -> ((d' !! i ==  f' && d !! i == f) || noSharedEOrbits (d !! i) f)
                                                && index ((d' !! i) `mappend` f') == index ((d !! i) `mappend` f))
                                          [0..length d' - 1]

obstruct' :: CTD -> CTD -> CGen -> Heuristics -> [Decomposition]
obstruct' ctd ctd' cg' heuristics = let decomps = decompositions ctd ctd' cg' heuristics
                                    in filter (\d -> bullet2 d && bullet3 d) decomps

--Accessor functions to obstruct and with (from LessThans)
thm119 :: CTD -> CTD -> CGen -> Maybe Decomposition
thm119 ctd ctd' cg' = obstruct ctd ctd' cg' defaultHeuristics

thm119' :: CTD -> CTD -> CGen -> [Decomposition]
thm119' ctd ctd' cg' = obstruct' ctd ctd' cg' defaultHeuristics

thm114 :: Double -> Double -> CGen -> Maybe Decomposition
thm114 a c cg' = obstruct (Polydisk a 1) (Ellipsoid c c) cg' `with` thm114Heuristics

thm114' :: Double -> Double -> CGen -> [Decomposition]
thm114' a c cg' = obstruct' (Polydisk a 1) (Ellipsoid c c) cg' `with` thm114Heuristics
--}}}
--Scanning {{{
--Normal (non-squeeze) scan
normalOne :: (Double -> CTD) -> (Double -> CTD) -> CGen -> [Double] -> [[Double]] -> Heuristics ->
             [(CGen,Double,Double,Maybe Decomposition)]
normalOne ctds ctds' cg' as cs heuristics =
                                let acs = zip as cs
                                in concat . takeWhile1 (\l -> length l > 1) $ map (\(a,cs) -> 
                                      takeWhile1 (\(_,_,_,result) -> isNothing result) $ map (\c ->
                                                 (cg',a,c,obstruct (ctds a) (ctds' c) cg' heuristics)) cs)
                                                                   acs

normalScan :: ScanDomain -> Heuristics -> [(CGen,Double,Double,Maybe Decomposition)]
normalScan domain heuristics = concatMap (\cg -> map useoffsets $ normalOne (getCTDs domain) (getCTDs' domain)
                                                                            cg as cs heuristics)
                                         (getCGs domain)
                               where (as,cs) = makeRange11 (getARange domain) (getMidC domain) (getCRange domain)
                                     midc = getMidC domain
                                     useoffsets (cg,a,c,r) = (cg,a,roundOff (c - midc a),r)

--Squeeze scan
squeezeOne :: Heuristics -> (Double -> CTD) -> (Double -> CTD) -> CGen -> (Double,Double,Double) ->
            (Double -> Double) -> (Double,Double,Double) -> [(CGen,Double,Double,Maybe Decomposition)]
squeezeOne heuristics ctds ctds' cg' (mina,maxa,astep) midc (minc,maxc,cstep) = 
      let (as,coffs) = (makeRange mina maxa astep,makeRange minc maxc cstep)
          (firsta,firstc) = (head as, midc (head as))
          (goodoffs,badoffs) = squeezeLower heuristics (ctds firsta) ctds' cg' firstc coffs
          goodends = squeezeUpper heuristics ctds ctds' cg' (tail as) midc goodoffs
          badresults = map (\(off,d) -> (cg',firsta,off,Just d)) badoffs
          goodresults = map (\(a,off,d) -> (cg',a,off,Just d)) $ goodends
          --TODO: Any offsets which work for all a values in the scan range will not appear
          --in results. Should they?
      in if length badresults == 0 && length goodresults == 0
         then [(cg',last as,last coffs,Nothing)]
         else badresults ++ goodresults

--squeezeScan helper functions {{{
squeezeLower :: Heuristics -> CTD -> (Double -> CTD) -> CGen -> Double -> [Double] ->
                ([Double],[(Double,Decomposition)])
squeezeLower heuristics ctd ctds' cg' midc coffs =
                             let cs = offsWithCTDs midc coffs ctds'
                                 decomps = decompositions ctd (snd $ last cs) cg' `with` heuristics 
                                 (left,found) = allPartition (\d (off,ctd') -> not $ satisfiesAConds d ctd ctd')
                                                             (filter (\d -> bullet2 d && bullet3 d) decomps) cs
                             in (map fst left,map (\((o,c),d) -> (o,d)) found)

squeezeUpper :: Heuristics -> (Double -> CTD) -> (Double -> CTD) -> CGen ->
                [Double] -> (Double -> Double) -> [Double] -> [(Double,Double,Decomposition)]
squeezeUpper _ _ _ _ _ _ [] = []
squeezeUpper _ _ _ _ [] _ _ = []
squeezeUpper heuristics ctds ctds' cg' as midc coffs =
             let decomps = decompositions (ctds $ last as) (ctds' . roundOff $ midc (last as) + last coffs)
                                          cg' `with` heuristics 
             in map (\(o,(a,d)) -> (a,o,d)) . Map.toList $
                foldl' (\acc d -> let newfound = snd $ allPartition (\a off -> not $ satisfiesAConds d (ctds a)
                                                                                         (ctds' (midc a + off)))
                                                                    as coffs
                                  in newMins acc newfound d)
                       Map.empty (filter (\d -> bullet2 d && bullet3 d) decomps)

newMins :: Map.Map Double (Double,Decomposition) -> [(Double,Double)] -> Decomposition ->
           Map.Map Double (Double,Decomposition)
newMins oldmap newfound decomp = foldl' (\acc (off,a) -> Map.insertWith insertfn off (a,decomp) acc)
                                        oldmap newfound
                                 where insertfn (oa,od) (na,nd) = if na < oa then (na,nd) else (oa,od)

testOffsets :: Decomposition -> CTD -> [(t,CTD)] -> ([(t,Decomposition)],[(t,CTD)])
testOffsets decomp ctd offs = foldr (\(off,ctd') (b,g) -> if satisfiesAConds decomp ctd ctd'
                                                          then ((off,decomp):b,g)
                                                          else (b,(off,ctd'):g))
                                    ([],[]) offs

satisfiesAConds :: Decomposition -> CTD -> CTD -> Bool
satisfiesAConds (lambda',lambda) ctd ctd' = all (\i -> action ctd (lambda !! i) <= action ctd' (lambda' !! i))
                                                [0..length lambda - 1]

offsWithCTDs :: Double -> [Double] -> (Double -> CTD) -> [(Double,CTD)]
offsWithCTDs midc offs ctds' = map (\off -> (off,ctds' (roundOff $ midc+off))) offs
--}}}

squeezeScan :: ScanDomain -> Heuristics -> [(CGen,Double,Double,Maybe Decomposition)]
squeezeScan domain heuristics = concatMap (\cg -> squeezeOne heuristics (getCTDs domain) (getCTDs' domain)
                                                       cg (getARange domain) (getMidC domain) (getCRange domain))
                                          (getCGs domain)
--}}}
--Bounding {{{
--Factorization works the same when bounding, but because we need to use
--generateLessThan without caring about the action condition, decompositions
--and generateLessThan must change. So, all the functions here are copies
--of those in "Factorizations, Decompositions, and Theorem 1.19" above which
--don't ask for the second CTD, because they don't need it. (The first CTD
--is still used for the maxYs heuristic.)
--_decompositionsB :: Heuristics -> CTD -> CGen -> [Decomposition]
--_decompositionsB heuristics ctd cg' = let facts' = factorizations cg'
--                                      in foldl (\acc f -> (_decompsB heuristics ctd f) ++ acc) [] facts'

--_decompsB :: Heuristics -> CTD -> [CGen] -> [Decomposition]
--_decompsB heuristics ctd fact =
--                   let lts = foldr (\f acc -> (_generateLessThanB heuristics ctd f) : acc) [] fact
--                   in map (\f -> (fact,MS.toList f)) $ MS.sequenceMS (MS.fromListEq lts)

--_thm119B :: Heuristics -> CTD -> CGen -> (CGen -> Double -> Double) -> Double
--_thm119B heuristics ctd cg' cfn = let decomps = filter (\d -> bullet2 d && bullet3 d) $
--                                                       _decompositionsB heuristics ctd cg'
--                                      cvals = map (\(fs,lts) -> map (\i -> cfn (fs !! i) $ action ctd (lts !! i))
--                                                                    [0..length fs - 1]) decomps
--                                  in minimum $ map maximum cvals
                                                
--boundNormalOver :: (Double,Double,Double) -> (Double -> CTD) -> [CGen] -> Heuristics ->
--                   (CGen -> Double -> Double) -> [(CGen,Double,Double)]
--boundNormalOver (mina,maxa,astep) ctds cgs heuristics cfn =
--                  let as = makeRange mina maxa astep
--                  in concatMap (\cg -> map (\a -> (cg,a,_thm119B heuristics (ctds a) cg cfn)) as) cgs

--boundSqueezeOver :: (Double,Double,Double) -> (Double -> CTD) -> [CGen] -> Heuristics ->
--                   (CGen -> Double -> Double) -> [(CGen,Double,Double)]
--boundSqueezeOver (mina,maxa,astep) ctds cgs heuristics cfn =
--                      concatMap (\cg -> let lower = _thm119B heuristics (ctds mina) cg cfn
--                                            upper = _thm119B heuristics (ctds maxa) cg cfn
--                                        in [(cg,mina,lower),(cg,maxa,upper)]) cgs
--}}}
