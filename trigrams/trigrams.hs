import Control.Monad
import Data.Char
import System.Environment
import System.IO
import System.Directory
import qualified Data.Map as Map

lookupItem :: Maybe [x] -> [x]
lookupItem Nothing = []
lookupItem (Just [x]) = [x]

processFile :: [String] -> Map.Map String [String] -> Map.Map String [String]
processFile [a, b] m = m
processFile (x:xs) m = processFile xs (Map.insertWith (++) (filter (/='"') x) [(unwords (take 2 xs))] $ m)

createTrigramText :: Map.Map String [String] -> String -> String
createTrigramText m [] = createTrigramText m ("Whenever " ++ (head $ lookupItem $ Map.lookup "whenever" m))
createTrigramText m x
  | textLength <= 400 = createTrigramText m (x ++ " " ++ (tail $ words $ tail x) ++ " " ++ (head $ lookupItem $ Map.lookup (tail $ words $ tail x) m))
  | otherwise = m
  where textLength = (length x)

main = do (fileName:_) <- getArgs
          contents <- readFile fileName
          let processedFile = processFile (words contents) Map.empty
          putStrLn (unwords (createTrigramText processedFile []))

