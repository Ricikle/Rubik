{-# LANGUAGE ViewPatterns #-}

module Stage2 where
  import MapHelper
  import Cube
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.State
  import Control.Monad

  main :: IO ()
  main = writeTable "Stage2.dat" generateTable2

  movesStage2 :: [Move]
  movesStage2 = [U, U', U2, L2, R2, D, D', D2, B, B', B2, F, F', F2]

  getMoveListCorner :: Table -> Cube -> (Cube,[Move])
  getMoveListCorner ma c = if cornerO c == fromList zero8
    then (c,[]) else let m = ma Map.! cornerO c
                         (a,b) = getMoveListCorner ma (apply [m] c)
                     in (a,m:b)

  generateTable2 :: Table
  generateTable2 = execState (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> State Table ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM movesStage2 $ \m -> do
        let c = apply [m] x
        ma <- get
        if Map.member (cornerO c) ma
          then return []
          else modify ( Map.insert (cornerO c) (invert m)) >> return [c]
      bfs (xs S.>< S.fromList (concat ys) )
