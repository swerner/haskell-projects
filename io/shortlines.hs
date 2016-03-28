import Control.Monad
import Data.Char

main = interact $ unlines . filter ((<10) . length) . lines
