module Matrix where

import Data.Function ((&))
import Streamly
import Data.List (transpose)
import Streamly.Prelude ((|:), nil)
import qualified Streamly.Prelude as S
import Control.Monad

type Matrix = [[Int]]

binMatrices :: Int -> [Matrix]
binMatrices n = [0, 1] & replicateM n . replicateM n

symmetric :: Matrix -> Bool
symmetric m = m == transpose m

balanced :: Matrix -> Bool
balanced m = balancedRows m && balancedRows (transpose m)

balancedRows :: Matrix -> Bool
balancedRows m = all (countOnes $ balance m) m
 where
  countOnes n = (== n) . length . filter (== 1)
  balance m = length m `div` 2

balMat :: Int -> IO [Matrix]
balMat n =
  S.toList
    .  asyncly
    $  S.fromFoldable (binMatrices n)
    |& S.filter symmetric
    |& S.filter balanced

pythTriples :: Int -> IO ()
pythTriples n = runStream . wAsyncly $ do
  let range = S.fromFoldable [1 .. n]
  x <- range
  y <- range
  let z = x ^ 2 + y ^ 2
  S.once $ when (isSquare z) $ print (x, y, intRoot z)
 where
  isSquare n =
    let root      = sqrt . fromIntegral
        floorRoot = fromIntegral . floor . root
    in  root n == floorRoot n
  intRoot = floor . sqrt . fromIntegral
