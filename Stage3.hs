{-# LANGUAGE ViewPatterns #-}

module Stage3 where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.State
  import Control.Monad
  import Data.Array

  main :: IO ()
  main = readTable "Stage3.dat" >>= putStrLn.show.Map.size

  movesStage3 :: [Move]
  movesStage3 = [U, U', U2, L2, R2, D, D', D2, B2, F2]

  arrEdge :: Array Int Int
  arrEdge = listArray (1,12) [1,2,1,2,1,2,1,2,3,3,3,3]

  arrCorner :: Array Int Int
  arrCorner = listArray (1,8) [1,2,1,2,2,1,2,1]

  getStage3Orientation :: Cube -> Orientation
  getStage3Orientation c = fromList ([arrEdge ! k | k <- toListP (edges c)]++[arrCorner ! k | k <- toListP (corner c)])

  generateTable3 :: Table
  generateTable3 = execState (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> State Table ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM movesStage3 $ \m -> do
        let c = apply [m] x
        ma <- get
        let o = getStage3Orientation c
        if Map.member o ma
          then return []
          else modify ( Map.insert (o) (invert m)) >> return [c]
      bfs (xs S.>< S.fromList (concat ys) )
