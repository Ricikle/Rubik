{-# LANGUAGE ViewPatterns #-}

module Stage1(writeTable,getMoveList)where
  import Cube
  import MapHelper
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad
  import Control.Monad.Trans.State
  import Control.Monad.IO.Class

  getMoveList :: Cube -> IO (Cube,[Move])
  getMoveList c' = do
    ma' <- readTable "Stage1.dat"
    return $ getMoves ma' c' where
      getMoves ma c = if edgeO c == fromList zero12
        then (c,[]) else let m = ma Map.! edgeO c
                             (a,b) = getMoves ma (apply [m] c)
                         in (a,m:b)

  writeTable :: IO ()
  writeTable = generateTable >>=
    writeTableToFile "Stage1.dat"

  generateTable :: IO (Table Orientation)
  generateTable = do
    putStr "          Objects in Queue"
    t <- execStateT (bfs (S.singleton identity)) Map.empty
    putStrLn "\rFinished Writing Stage 1  "
    return t where
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
        liftIO . putStr $ '\r' : (show . length $ zs)
        bfs (zs)
