{-# LANGUAGE ViewPatterns #-}

module Stage1 where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.State
  import Control.Monad

  main :: IO ()
  main = do
    x <- readTable "Stage1.dat"
    putStrLn $ show $ x == generateTable


  generateTable1 :: Table
  generateTable = execState (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> State (Table) ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM moves $ \m -> do
        let c = apply [m] x
        ma <- get
        if Map.member (edgeO c) ma
          then return []
          else modify ( Map.insert (edgeO c) (invert m)) >> return [c]
      bfs (xs S.>< S.fromList (concat ys) )
