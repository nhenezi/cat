import System.IO
import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStr

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [] = getContents
parse fs = concat `fmap` mapM readFile fs


usage = putStrLn "Usage cat [OPTION]... [FILE]..."
version = putStrLn "Cat v0.1"

exit = exitWith ExitSuccess
