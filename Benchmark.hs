import CIP
import Criterion.Main

cs = map (\i -> pathFromEdges [(i,-i)]) [1..]
gs = map makeAllEGen cs
ctd1 = Polydisk 2.5 1
ctd2 = Ellipsoid 3.2 3.2
ctd3 = Ellipsoid 3.3 3.3

main = defaultMain [
          bgroup "thm119" [ bench "9-2" $ nf (decompositions ctd1 ctd2) (gs !! 8)
                          , bench "10-2" $ nf (thm119 ctd1 ctd2) (gs !! 9)
                          , bench "9-3" $ nf (decompositions ctd1 ctd3) (gs !! 8)
                          , bench "10-3" $ nf (thm119 ctd1 ctd3) (gs !! 9)
                          ]
                   ]

