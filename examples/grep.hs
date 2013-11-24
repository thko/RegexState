import System.Environment (getArgs)
import System.Exit
import System.IO
import Control.Monad
import RegexState

usage :: IO ()
usage = hPutStrLn stderr "Usage: grep PATTERN [FILE..]"

main :: IO ()
main = do
  args <- getArgs
  when (null args) (usage >> exitFailure)
  let pattern:files = args
  case compileRegex pattern >>= \r -> return $ grepFiles r files of
    Left  errmsg  -> hPutStrLn stderr errmsg
    Right matches -> matches >>= mapM_ putStrLn

grep :: RegexState -> [String] -> [String]
grep = filter . match

grepFiles :: RegexState -> [FilePath] -> IO [String]
grepFiles r files = case files of
  []     -> getContents >>= return . grep r . lines
  [file] -> readFile file >>= return . grep r . lines
  fs  -> mapM (readFile >=> return . grep r . lines) fs >>= return . concat
