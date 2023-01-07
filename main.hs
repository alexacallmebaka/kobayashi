import System.IO
import Text.Parsec (ParseError)
import Data.Either (partitionEithers)
import Control.Monad
import Lexer
import Parser
import HTML

--first lex, then parse or return errors.
buildSite :: String -> Either ParseError [Element]
buildSite [] = Right []
buildSite x = case tokenize "index.mai" x of
                Left l -> Left l
                Right r -> parse "index.mai" r

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  site <- return $ map buildSite $ lines $ contents
  case partitionEithers site of
    ([],y) -> do
      putStrLn "<!DOCTYPE HTML>\n<html>\n<body>"
      mapM_ (putStrLn . htmlFold) y
      putStrLn "</body>\n</html>"
    (x,y) -> do
      putStrLn "Some errors have occured!" 
      mapM_ print x
  hClose handle 
