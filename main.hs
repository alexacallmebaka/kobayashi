import System.IO

import Control.Applicative hiding ((<|>),many)
import Lexer
--import Web

--parseFile :: String -> [Token]
--parseFile contents = lexFile "index.mai" contents

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  case tokenize "index.mai" contents of 
    Left e -> print e
    Right e -> print e
    --Right r -> putStr $ foldl (\acc x -> acc ++ htmlify x ++ "\n") "" r
  hClose handle
