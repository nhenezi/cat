import System.IO
import System.Environment
import System.Exit
import Text.Printf
import Data.List
import Data.Functor

main :: IO ()
main = getArgs >>= parse >>= putStr

data Flag = LineNumber | EndLine | None deriving (Eq, Show)
type FileContent = String

parse :: [String] -> IO String
parse ["-h"] = usage >> exit
parse ["--help"] = usage >> exit
parse ["-v"] = version >> exit
parse ["--version"] = version >> exit
parse [] = getContents
parse fs = do
  let (flags, files) = processArguments fs
  contents <- mapM readFile files
  return $ processFiles flags contents

processArguments :: [String] -> ([Flag], [FilePath])
processArguments ls = (flags, files)
  where
    flags = nub $ filter (\x -> x /= None) (fst <$> res)
    files = filter (\x -> x /= "") (snd <$> res)
    res = processArgument <$> ls

processArgument :: String -> (Flag, FilePath)
processArgument "-n" = (LineNumber, "")
processArgument "--number" = (LineNumber, "")
processArgument "-E" = (EndLine, "")
processArgument "--show-ends" = (EndLine, "")
processArgument s = (None, s)

processFiles :: [Flag] -> [FilePath] -> FileContent
processFiles flags contents = foldl applyFlag (concat contents) flags

applyFlag :: FileContent -> Flag -> FileContent
applyFlag s LineNumber = unlines $ zipWith (\n line -> (printf "%6d  %s" n line)::String) [1::Integer ..] (lines s)
applyFlag s EndLine = unlines $ map (\line -> (printf "%s$" line)::String) (lines s)
applyFlag s _ = s


usageText = unlines [
  "Usage cat [OPTION]... [FILE]...",
  "Concatenate FILE(s), or standard input, to standard output",
  "",
  "-E, --show-ends     display dolar sign ($) at the end of each line",
  "-h, --help          display this help and exit",
  "-n, --number        display line numbers",
  "-v, --version       output version information and exit"
  ]



usage = putStrLn usageText
version = putStrLn "Cat v0.1"

exit = exitWith ExitSuccess
