import Data.Word8 (Word8)
import Data.Char (ord, chr)
import System.Environment (getArgs)
import Control.Monad (when)
import System.IO
  ( readFile, putChar, getChar, hPutStrLn,
    stdout, stdin, stderr,
    hSetBuffering, BufferMode (NoBuffering) )
import System.Exit (exitFailure)

--represents a bf instruction or a loop
data Instruction = Plus
                 | Minus
                 | Next
                 | Prev
                 | Input
                 | Output
                 | Loop [Instruction]
                 | MDump
                 | PDump
                 deriving (Show, Eq)
type Program = [Instruction]

--the memory zipper: (left, pointer:right)
type Memory = ([Word8], [Word8])

--the default memory, an infinite zipper filled with zeros
defaultMemory :: Memory
defaultMemory = (repeat 0, repeat 0)

--breaks a string when the brackets are balanced
--an open bracket adds 1 to the balance, a closed one subtracts 1
breakWhenBalanced :: Int -> String -> (String, String)
breakWhenBalanced 0 str = ("",str)
breakWhenBalanced balance (s:str)
  | s == '['  = let (first, second) = breakWhenBalanced (balance+1) str
                in (s:first, second)
  | s == ']'  = let (first, second) = breakWhenBalanced (balance-1) str
                in (s:first, second)
  | otherwise = let (first, second) = breakWhenBalanced balance str
                in (s:first, second)
breakWhenBalanced balance [] =
  error $ "malformed code, brackets balance off by " ++ show balance


--parse a string to a Program
bfParse :: String -> Program -> Program
bfParse "" prog = prog --edge condition
--adds a Loop containing the code inside the brackets
bfParse ('[':str) prog = bfParse str' $ prog ++ [Loop loop]
  where loop = bfParse (init loopStr) []
        (loopStr, str') = breakWhenBalanced 1 str
--all other instructions
bfParse (char:str) prog = bfParse str $ prog ++ instruction
  where instruction = case char2instruction char
                      of Just a -> [a]
                         Nothing -> [] --comments

char2instruction :: Char -> Maybe Instruction
char2instruction '>' = Just Next
char2instruction '<' = Just Prev
char2instruction '+' = Just Plus
char2instruction '-' = Just Minus
char2instruction '.' = Just Output
char2instruction ',' = Just Input
char2instruction '#' = Just MDump
char2instruction '§' = Just PDump --i picked a random not-widely-used character for this
char2instruction  _  = Nothing --comments

--the actual interpreter
bf :: Program -> Memory -> IO Memory
bf [] memory = return memory --end of (sub)program
--move the pointer
bf (Next:commands) (ml, m:mr) = bf commands (m:ml, mr)
bf (Prev:commands) (m:ml, mr) = bf commands (ml, m:mr)
--change the pointed byte
bf (Plus:commands) (ml, m:mr) = bf commands (ml, (m+1):mr)
bf (Minus:commands) (ml, m:mr) = bf commands (ml, (m-1):mr)
--output of pointed byte
bf (Output:commands) (ml, m:mr) = do
  putChar $ chr $ fromIntegral m
  bf commands (ml, m:mr)
--input to pointed byte
bf (Input:commands) (ml, _:mr) = do
  char <- getChar
  let m = fromIntegral $ ord char
  bf commands (ml, m:mr)
--loop (brackets)
bf ((Loop loop):commands) (ml, m:mr)
  | m == 0 = bf commands (ml, m:mr) --skip the loop
  | otherwise = do
    memory' <- bf loop (ml, m:mr) --execute the loop one time
    bf ((Loop loop):commands) memory' --recurse
    -- The aboe code isn't tail recursive, but it's actually faster than the
    -- following:
    -- > bf (loop ++ (Loop loop):commands) (ml, m:mr)
    -- This is probably because doing it like this ↑
    -- we traverse the list multiple times per loop.

--debug
--memory dump ('#', according to Urban Müller's original interpreter)
bf (MDump:commands) (ml,m:mr) = do
  hPutStrLn stderr "Memory dump:"
  -- This is not ideal. To do this correctly we should keep track of what
  -- was evaluated so far
  hPutStrLn stderr $ "  "
                  ++ show (takeWhile (/=0) ml)
                  ++ " >"
                  ++ show m
                  ++ "< "
                  ++ show (takeWhile (/=0) mr)
  bf commands (ml,m:mr)
--program dump
bf (PDump:commands) memory = do
  hPutStrLn stderr "Program dump:"
  hPutStrLn stderr $ "  " ++ show commands
  bf commands memory
--catch-all pattern. I have yet to discover a way to fall down there
--whitout the infinite zipper it would be out of memory
bf _ _ = error "Impossible error. Please report a bug if this gets printed."


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn "Usage: yabi path"
    exitFailure
  rawProgram <- readFile $ head args
  hPutStrLn stderr "[yabi] Parsing..."
  --parse the program string
  let program = bfParse rawProgram []
  hPutStrLn stderr "[yabi] Parsed. Executing..."
  --disable buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  --start the interpreter
  --we need the side effects, not the final memory, hence `_ <-`
  _ <- bf program defaultMemory
  return ()

