import System.IO
import System.Random
import System.Environment
import Data.Random
import Data.Random.Extras
import Data.Random.Source.DevRandom
import qualified Data.Map as Map

lookupItem :: Maybe [String] -> [String]
lookupItem Nothing = []
lookupItem (Just a) = a

processFile :: [String] -> Map.Map String [String] -> Map.Map String [String]
processFile [a, b] m = Map.insertWith (++) (filter (/='"') a) [b] $ m
processFile (x:xs) m = processFile xs (Map.insertWith (++) (filter (/='"') x) [(unwords (take 2 xs))] $ m)

createTrigramText :: Map.Map String [String] -> [String] -> [Int] -> [String]
createTrigramText m x randoms
  | (length x) <= 200 =
    createTrigramText m (x ++ [item grams]) (tail randoms)
  | otherwise = x
  where
    grams = lookupItem $ Map.lookup (last $ words $ last x) m
    item g
      | (length g) > 0 = g !! ((head randoms) `mod` (length g))
      | otherwise = (Map.keys m) !! ((head randoms) `mod` (length $ Map.keys m))

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let processedFile = processFile (words contents) Map.empty
          g <- getStdGen
          let rands = take 1000 (randoms g :: [Int])
          firstWord <- runRVar (Data.Random.Extras.choice (Map.keys processedFile)) DevRandom
          putStrLn ( unwords ( createTrigramText processedFile [firstWord] rands))
