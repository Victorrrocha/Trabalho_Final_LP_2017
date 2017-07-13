import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word

data BFInstruction = GoBack | GoForward | Increment | Decrement| Input
                   | Output | Loop [BFInstruction]
                   deriving (Show)
                   
parseBack, parseForward, parseIncrement, parseLoop,
 parseDecrement, parseInput, parseOutput :: Parser BFInstruction
 
parseGen :: Char -> BFInstruction -> Parser BFInstruction
parseGen x y = char x >> return y
 
parseBack = parseGen '<' GoBack
parseForward = parseGen '>' GoForward
parseIncrement = parseGen '+' Increment
parseDecrement = parseGen '-' Decrement
parseInput = parseGen ',' Input
parseOutput = parseGen '.' Output
 
parseLoop = do 
   char '['
   insn <- parseInstructions
   char ']'
   return $ Loop insn
   
parseComment :: Parser ()
parseComment  =  do 
  many $ noneOf "<>+-,.[]"
  return ()
   
parseInstruction :: Parser BFInstruction
parseInstruction = do
  parseComment
  i <- parseBack <|> parseForward <|> parseIncrement <|> parseDecrement
    <|> parseInput <|> parseOutput <|> parseLoop
  parseComment
  return i
  
parseInstructions :: Parser [BFInstruction]
parseInstructions = many parseInstruction

type BFRunner = StateT (Int, M.IntMap Word8) IO ()

zeroise :: Maybe Word8 -> Word8
zeroise =  maybe 0 id

runInstruction :: BFInstruction -> BFRunner
 
runInstruction GoBack = modify (\(h,m) -> (h-1, m))
runInstruction GoForward = modify (\(h,m) -> (h+1, m))
runInstruction Increment = do
  (bfHead, bfMap) <- get
  let val = zeroise (M.lookup bfHead bfMap)
  put (bfHead, M.insert bfHead (val + 1) bfMap)  
runInstruction Decrement = do
  (bfHead, bfMap) <- get
  let val = zeroise (M.lookup bfHead bfMap)
  put (bfHead, M.insert bfHead (val - 1) bfMap)  
runInstruction Input = do
  (bfHead, bfMap) <- get
  c <- liftIO getChar
  put (bfHead, M.insert bfHead (fromIntegral (fromEnum c)) bfMap)
runInstruction Output = do
  (bfHead, bfMap) <- get
  let val = zeroise (M.lookup bfHead bfMap)
  liftIO $ putChar $ toEnum $ fromIntegral val
runInstruction loop@(Loop insns) = do
  (bfHead, bfMap) <- get
  let val = zeroise (M.lookup bfHead bfMap)
  case val of
    0 -> return ()
    _ -> runInstructions insns >> runInstruction loop
  
runInstructions :: [BFInstruction] -> BFRunner
runInstructions = mapM_ runInstruction

derivacao :: Parser [BFInstruction]
derivacao der = 
  ler <- readFile "hello.bf"
  case parse parseInstructions "hello.bf" ler of
    Left 
    Right insn ->  print insn - 


main :: IO ()
main = do
  cont <- readFile "hello.bf"
  case parse parseInstructions "hello.bf" cont of
    Left insn -> print insn
    Right insns -> evalStateT (runInstructions insns) (0, M.empty)
    --Right insn -> print insn + parseInstructions