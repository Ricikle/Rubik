{-# LANGUAGE ViewPatterns #-}

module Stage1 where
  import Cube
  import qualified Data.Map.Lazy as Map
  import qualified Data.Sequence as S
  import Control.Monad.State
  import Control.Monad

  main :: IO ()
  main = do
    x <- readTable
    putStrLn $ show $ x == generateTable

  getMoveList :: Map.Map Orientation Move -> Orientation -> [Move]
  getMoveList ma o = if o == fromList [0,0,0,0,0,0,0,0,0,0,0,0]
    then [] else let m = ma Map.! o in m : getMoveList ma (moveOrientationEdge o m)


  generateTable :: Map.Map Orientation Move
  generateTable = execState (bfs (S.singleton identity)) Map.empty where
    bfs :: S.Seq Cube -> State (Map.Map Orientation Move) ()
    bfs (S.viewl -> S.EmptyL) = return ()
    bfs (S.viewl -> (x S.:< xs)) = do
      ys <- forM moves $ \m -> do
        let c = apply [m] x
        ma <- get
        if Map.member (edgeO c) ma
          then return []
          else modify ( Map.insert (edgeO c) (invert m)) >> return [c]
      bfs (xs S.>< S.fromList (concat ys) )

  writeTable :: IO ()
  writeTable = writeFile "Stage1.dat" $ Map.foldrWithKey f "" generateTable where
    f k a b = show (toListO k) ++ ' ' : show a ++  '\n' : b

  readTable :: IO (Map.Map Orientation Move)
  readTable = do
    s <- readFile "Stage1.dat"
    let ls = lines s
    return $ foldr f Map.empty ls where
      f x y = let a:b:_ = words x in Map.insert (fromList (read a)) (read b) y
