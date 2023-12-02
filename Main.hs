import System.Environment
import Text.ParserCombinators.Parsec
import Parser
import Evaluation

main :: IO ()
main =
  do
    sourceFileName:_ <- getArgs
    source <- readFile sourceFileName
    let programOrParseError = parse programParser "" source
    putStrLn . show $ evaluate <$> programOrParseError
