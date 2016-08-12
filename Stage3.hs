{-# LANGUAGE ViewPatterns #-}

module Stage3 (writeTable,getMoveList) where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.Trans.State
  import Control.Monad
  import Control.Monad.IO.Class
  import Data.Array

  movesStage3 :: [Move]
  movesStage3 = [U, U', U2, L2, R2, D, D', D2, B2, F2]

  getMoveList :: Cube -> IO (Cube,[Move])
  getMoveList c = readTable "Stage3.dat" >>= \t ->
    return $ getMoveListStage3 t c

  getMoveListStage3 :: Table Arr -> Cube -> (Cube,[Move])
  getMoveListStage3 ma c = if getStage3Arr c == getStage3Arr identity
    then (c,[]) else let m = ma Map.! getStage3Arr c
                         (a,b) = getMoveListStage3 ma (apply [m] c)
                     in (a,m:b)

  arrEdge :: Array Int Int
  arrEdge = listArray (1,12) [1,2,1,2,1,2,1,2,3,3,3,3]

  arrCorner :: Array Int Int
  arrCorner = listArray (1,8) [1,2,1,2,2,1,2,1]

  getStage3Arr :: Cube -> Arr
  getStage3Arr c = fromList ([arrEdge ! k | k <- toList (edges c)]++[arrCorner ! k | k <- toList (corner c)])

  writeTable :: IO ()
  writeTable = generateTable >>=
    writeTableToFile "Stage3.dat"

  generateTable :: IO (Table Arr)
  generateTable = execStateT (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> StateT (Table Arr) IO ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM movesStage3 $ \m -> do
        let c = apply [m] x
        ma <- get
        let o = getStage3Arr c
        if Map.member o ma
          then return []
          else modify ( Map.insert (o) (invert m)) >> return [c]
      let zs = xs S.>< S.fromList (concat ys)
      liftIO . putStrLn . show . length $ zs
      bfs (zs)
