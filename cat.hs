import System.IO
import System.Environment
import System.Exit
import Text.Printf
import Data.List

main :: IO ()
main = getArgs >>= parse >>= putStr

data Flag = LineNumber | None deriving (Eq, Show)

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

processArguments :: [String] -> ([Flag], [String])
processArguments ls = (flags, files)
  where
    flags = nub $ filter (\x -> x /= None) (map fst res)
    files = filter (\x -> x /= "") (map snd res)
    res = map processArgument ls

processArgument :: String -> (Flag, String)
processArgument "-n" = (LineNumber, "")
processArgument "--number" = (LineNumber, "")
processArgument s = (None, s)

processFiles :: [Flag] -> [String] -> String
processFiles flags contents = foldl applyFlag (concat contents) flags

applyFlag :: String -> Flag -> String
applyFlag s LineNumber = unlines $ zipWith (\n line -> (printf "%6d %s" n line)::String) [1::Integer ..] (lines s)
applyFlag s _ = s


usageText = unlines [
  "Usage cat [OPTION]... [FILE]...",
  "Concatenate FILE(s), or standard input, to standard output",
  "",
  "-h, --help     display this help and exit",
  "-n, --number   display line numbers",
  "-v, --version  output version information and exit"
  ]



usage = putStrLn usageText
version = putStrLn "Cat v0.1"

exit = exitWith ExitSuccess
