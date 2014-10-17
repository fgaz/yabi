import Data.Word8 (Word8)
import Data.Char (ord, chr)
import System.Environment (getArgs)
import System.IO (readFile, putChar, getChar, stdout, stdin, hSetBuffering, BufferMode (NoBuffering))
import System.Exit (exitFailure)

--the memory & program zippers
type Memory = ([Word8],[Word8]) -- the memory zipper: (left, pointer:right)
type Program = ([Char],[Char]) -- the instructions zipper: (todo,done)

--the default memory, an infinite zipper filled with zeros
defaultMemory :: ([Word8],[Word8])
defaultMemory = ((repeat 0),(repeat 0))

--breaks a string when the brackets are balanced
-- an open bracket adds 1 to the balance, a closed one subtracts 1
breakWhenBalanced :: Int -> String -> (String, String)
breakWhenBalanced 0 str = ("",str)
breakWhenBalanced balance (s:str) | s == '[' = let (first, second) = breakWhenBalanced (balance+1) str in (s:first, second)
                                  | s == ']' = let (first, second) = breakWhenBalanced (balance-1) str in (s:first, second)
                                  | otherwise = let (first, second) = breakWhenBalanced balance str in (s:first, second)
breakWhenBalanced balance [] = error $ "malformed code, brackets balance off by " ++ (show balance)

--the actual interpreter
bf :: Program -> Memory -> IO ()
bf ([], _) _ = return () --end of program
--move the pointer
bf ('>':commands, done) (ml, m:mr) = bf (commands, '>':done) (m:ml, mr)
bf ('<':commands, done) (m:ml, mr) = bf (commands, '<':done) (ml, m:mr)
--change the pointed byte
bf ('+':commands, done) (ml, m:mr) = bf (commands, '+':done) (ml, (m+1):mr)
bf ('-':commands, done) (ml, m:mr) = bf (commands, '-':done) (ml, (m-1):mr)
--output of pointed byte
bf ('.':commands, done) (ml, m:mr) = do
                                       putChar $ chr $ fromIntegral m
                                       bf (commands, '.':done) (ml, m:mr)
--input to pointed byte
bf (',':commands, done) (ml, _:mr) = do
                                       char <- getChar
                                       let m = fromIntegral $ ord char
                                       bf (commands, ',':done) (ml, m:mr)
--if pointed byte == 0 then jump forward to the corresponding ]
bf ('[':commands, done) (ml, m:mr) | m == 0 = bf (commands', done') (ml, m:mr)
                                   | otherwise = bf (commands, '[':done) (ml, m:mr)
    where (token, commands') = breakWhenBalanced 1 commands
          done' = (reverse token) ++ '[':done
--if pointed byte /= 0 then jump back to the corresponding [
bf (']':commands, done) (ml, m:mr) | m /= 0 = bf (commands', done') (ml, m:mr)
                                   | otherwise = bf (commands, ']':done) (ml, m:mr) --the interpreter works even without this line and "| m /= 0" on the line above, but this way is faster
    where (token, done') = breakWhenBalanced (-1) done
          commands' = (reverse token) ++ ']':commands

--debug
--memory dump ('#', according to Urban Müller's original interpreter)
bf ('#':commands, done) (ml,m:mr) = do
                                   putStrLn "Memory dump:"
                                   putStrLn $ "  " ++ (show $ takeWhile (/=0) ml) ++ " >" ++ (show m) ++ "< " ++ (show $ takeWhile (/=0) mr)
                                   bf (commands, '#':done) (ml,m:mr)
--program dump (i picked a random not-widely-used character for this)
bf ('§':commands, done) (ml,mr) = do
                                   putStrLn "Program dump:"
                                   putStrLn $ "  " ++ (reverse done) ++ "|" ++ commands
                                   putStrLn $ (replicate (length done + 2) ' ') ++ "^"
                                   bf (commands, '§':done) (ml,mr)
--catch-all pattern. I have yet to discover a way to fall down there
bf _ _ = error "malformed code"

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        putStrLn "Usage: yabi path"
        exitFailure
    else return ()
    rawProgram <- readFile $ head args
    let program = (filter (`elem` "><+-.,[]#§") rawProgram, [])
    --disable buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    --start the interpreter
    bf program defaultMemory
