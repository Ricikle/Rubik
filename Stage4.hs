{-# LANGUAGE ViewPatterns #-}

module Stage4 where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.Trans.State
  import Control.Monad
  import Control.Monad.IO.Class

  main :: IO ()
  main = generateTable4 >>= \t -> writeTable "Stage4.dat" t

  movesStage4 :: [Move]
  movesStage4 = [U2, L2, R2, D2, B2, F2]

  getStage4Orientation :: Cube -> Orientation
  getStage4Orientation c = fromList ([k !> corner c | k <- [1..8]] ++ [k !> edges c | k <- [1..12]])


  generateTable4 :: IO (Table Orientation)
  generateTable4 = execStateT (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> StateT (Table Orientation) IO ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM movesStage4 $ \m -> do
        let c = apply [m] x
        ma <- get
        let o = getStage4Orientation c
        if Map.member o ma
          then return []
          else modify ( Map.insert (o) (invert m)) >> return [c]
      let zs = xs S.>< S.fromList (concat ys)
      liftIO . putStrLn . show . length $ zs
      bfs (zs)
