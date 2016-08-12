{-# LANGUAGE ViewPatterns #-}

module Stage2 (getMoveList,writeTable) where
  import MapHelper
  import Cube
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.Trans.State
  import Control.Monad
  import Control.Monad.IO.Class

  movesStage2 :: [Move]
  movesStage2 = [U, U', U2, L2, R2, D, D', D2, B, B', B2, F, F', F2]

  getMoveList :: Cube -> IO (Cube,[Move])
  getMoveList c = readTable "Stage2.dat" >>= \t ->
    return $ getMoveListCorner t c

  getMoveListCorner :: Table Orientation -> Cube -> (Cube,[Move])
  getMoveListCorner ma c = if cornerO c == fromList zero8
    then (c,[]) else let m = ma Map.! cornerO c
                         (a,b) = getMoveListCorner ma (apply [m] c)
                     in (a,m:b)

  writeTable :: IO ()
  writeTable = generateTable >>=
    writeTableToFile "Stage2.dat"

  generateTable :: IO (Table Orientation)
  generateTable = do
    putStr "          Objects in Queue"
    t <- execStateT (bfs (S.singleton identity)) Map.empty
    putStrLn "\rFinished writing Stage 2  "
    return t where
      bfs :: S.Seq Cube -> StateT (Table Orientation) IO ()
      bfs (S.viewl -> S.EmptyL) = return ()
      bfs (S.viewl -> (x S.:< xs)) = do
        ys <- forM movesStage2 $ \m -> do
          let c = apply [m] x
          ma <- get
          if Map.member (cornerO c) ma
            then return []
            else modify ( Map.insert (cornerO c) (invert m)) >> return [c]
        let zs = xs S.>< S.fromList (concat ys)
        liftIO . putStr $ '\r' : (show . length $ zs)
        bfs (zs)
