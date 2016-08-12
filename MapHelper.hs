module MapHelper where
  import Cube
  import qualified Data.Map.Lazy as Map

  type Table a = Map.Map a Move

  writeTableToFile :: (Show a) => FilePath -> Table a ->  IO ()
  writeTableToFile fp t = writeFile fp $ Map.foldrWithKey f "" t where
    f k a b = show k ++ ' ' : show a ++  '\n' : b

  readTable :: (Read a, Ord a) => FilePath -> IO (Table a)
  readTable fp = do
    s <- readFile fp
    let ls = lines s
    return $ foldr f Map.empty ls where
      f x y = let a:b:_ = words x in Map.insert (read a) (read b) y
