{-# LANGUAGE ViewPatterns #-}

module Stage1 where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad
  import Control.Monad.Trans.State
  import Control.Monad.IO.Class

  main :: IO ()
  main = do
    x <- readTable "Stage1.dat"
    y <- generateTable1
    putStrLn $ show $ x == y

  getMoveListEdge :: Table Orientation -> Cube -> (Cube,[Move])
  getMoveListEdge ma c = if edgeO c == fromList zero12
    then (c,[]) else let m = ma Map.! edgeO c
                         (a,b) = getMoveListEdge ma (apply [m] c)
                     in (a,m:b)


  generateTable1 :: IO (Table Orientation)
  generateTable1 = execStateT (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> StateT (Table Orientation) IO ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM moves $ \m -> do
        let c = apply [m] x
        ma <- get
        if Map.member (edgeO c) ma
          then return []
          else modify ( Map.insert (edgeO c) (invert m)) >> return [c]
      let zs = xs S.>< S.fromList (concat ys)
      liftIO . putStrLn . show . length $ zs
      bfs (zs)
