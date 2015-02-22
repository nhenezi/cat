import System.IO
import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStr

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs


usage_text = unlines [
  "Usage cat [OPTION]... [FILE]...",
  "Concatenate FILE(s), or standard input, to standard output",
  "",
  "-h, --help     display this help and exit",
  "-v, --version  output version information and exit"
  ]



usage = putStrLn usage_text
version = putStrLn "Cat v0.1"

exit = exitWith ExitSuccess
