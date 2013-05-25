import Criterion.Main
import Control.Arrow ((***))
import Control.Monad (join)
import Data.List (genericLength, mapAccumL)



main = defaultMain [
    bench "spark1_10" $ nf spark1 [1, 5, 6, 2, 6, 7, 2, 8, 4, 3, 6],
    bench "spark2_10" $ nf spark2 [1, 5, 6, 2, 6, 7, 2, 8, 4, 3, 6],
    bench "spark1_100" $ nf spark1 (replicate 100 2),
    bench "spark2_100" $ nf spark2 (replicate 100 2)
    ]

spark1 :: RealFrac b => [b] -> String
spark1 list = map ("▁▂▃▄▅▆▇" !!) xs
           where zs = map (flip (-) (minimum list)) list
                 xs = map (round . (* 6) . (/ maximum zs)) zs




spark2 :: RealFrac b => [b] -> String
spark2 = mapNormalize ((ticks!!) . round . (* (genericLength ticks - 1)))

ticks = " ▁▂▃▅▆▇"

mapNormalize :: (Ord a, Fractional a, Num a) => (a -> b) -> [a] -> [b]
mapNormalize f xs =
  let ((min',max'), runNormalize) =
        mapAccumL (\accumMinMax x ->
                    ((min x *** max x) accumMinMax,
                    f $ (x - min')/(max' - min')))
                  (join (,) $ head xs) xs
  in runNormalize
