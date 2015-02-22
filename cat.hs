module Cat where

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
  content <- mapM readFile files
  return $ concat $ fmap (processFile flags) content

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

applyArguments :: [String] -> [String]
applyArguments fs = map (processFile flags) files
  where (flags, files) = processArguments fs

processFile :: [Flag] -> String -> String
processFile flags file = foldr applyFlag file flags

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

applyFlag :: Flag -> String -> String
applyFlag LineNumber s = unlines $ zipWith (\n line-> (printf "%6d %s" n line)::String) [1::Integer ..] (lines s)
applyFlag _ s = s


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
