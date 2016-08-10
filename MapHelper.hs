module MapHelper where
  import Cube
  import qualified Data.Map.Lazy as Map

  type Table = Map.Map Orientation Move

  getMoveList :: Table -> Orientation -> [Move]
  getMoveList ma o = if o == fromList (replicate (maxNo o) 0)
    then [] else let m = ma Map.! o in m : getMoveList ma (moveOrientationEdge o m)

  writeTable :: FilePath -> Table ->  IO ()
  writeTable fp t = writeFile fp $ Map.foldrWithKey f "" t where
    f k a b = show (toListO k) ++ ' ' : show a ++  '\n' : b

  readTable :: FilePath -> IO (Table)
  readTable fp = do
    s <- readFile fp
    let ls = lines s
    return $ foldr f Map.empty ls where
      f x y = let a:b:_ = words x in Map.insert (fromList (read a)) (read b) y
