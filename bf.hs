import Text.Parsec
import Text.Parsec.String
import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word
import System.Environment
import System.Console.GetOpt
import System.Exit 
import Data.Maybe
import System.FilePath

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

-------------------------Options-------------------------
data Action = ParseOnly | Interpret | Bitcode deriving (Show, Eq)

data Options = Options
               { optHelp :: Bool
               , optVersion :: Bool
               , optAction :: Action
               }
               deriving (Show)

defaultOptions :: Options
defaultOptions = 
  Options{ optHelp = False
         , optVersion = False
         , optAction = Interpret
         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['v']    ["version"]
            (NoArg (\ opts -> opts { optVersion = True}))
            "show the version of the bf interpreter"
          , Option ['h']    ["help"]
            (NoArg (\ opts -> opts { optHelp = True}))
            "show help for the bf interpreter"
          , Option ['p']    ["parse"]
            (NoArg (\ opts -> opts { optAction = ParseOnly}))
            "parse the input and display it"
          , Option ['i']    ["interpret"]
            (NoArg (\ opts -> opts { optAction = Interpret}))
            "parse the input and interpret it"
          ]

usage :: String
usage = usageInfo header options
  where
    header = "Usage: bf [OPTION...] filename\n\n"          

bfOptions :: [String] ->IO (Options, Maybe String)
bfOptions argv =
  case getOpt Permute options argv of
    (o, [n], []  ) -> return (foldl (flip id) defaultOptions o, Just n)
    (o, _,   []  ) -> return (foldl (flip id) defaultOptions o, Nothing)
    (_, _,   errs) -> ioError $ userError $ concat errs ++ usage



main :: IO ()
main = do
  argv <- getArgs
  (opts, fname) <- bfOptions argv

  when (optVersion opts) $ do
    putStrLn "bf Version 1"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  when (fname == Nothing) $ do
    putStrLn "You must supply exactly one file name to bf\n"
    putStrLn usage
    exitFailure
  let fname' = fromJust fname
  let action = optAction opts
  cont <- readFile fname'
  case parse parseInstructions fname' cont of
    Left insn -> print insn
    Right insns -> do
      if action == ParseOnly
        then print insns
        else evalStateT (runInstructions insns) (0, M.empty)
      --case action of
         -- ParseOnly -> print insns
          --Interpret -> evalStateT (runInstructions insns) (0, M.empty)
          --Bitcode -> writeBitcodeFile insns (replaceExtension fname' ".bc")
    --Right insn -> print insn + parseInstructions