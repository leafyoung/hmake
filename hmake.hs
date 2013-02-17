module Main where

import Data.List (foldl', break, intersperse)
import Data.Char (toLower)
import Control.Monad
import Control.Monad.Error
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.Text as T
import System.Cmd
import GHC.IO.Exception
import System.Exit
import System.Environment (getArgs)
import System.Directory (doesFileExist)

data MakeVal = Atom String [String]
             | Rule String [String] [String]
             | Comment String 
             deriving (Show)

-- instance Show MakeVal where show = showVal

showVal (Atom name value) =
   "Value: " ++ show name ++ ": " ++ show value
showVal (Rule name elem comm) = 
   "Rule: " ++ show name ++ 
   " (deps:" ++ show elem ++ ") " ++ 
   "(cmd: " ++ show comm ++ ")"
showVal (Comment s) = 
   "Comment: " ++ show s

data MakeError = NoCommand String
               | Parser ParseError
               | Default String
               | UnboundVar String

instance Show MakeError where show = showError

showError (NoCommand name) = "No action for " ++ name
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default s) = s
showError (UnboundVar s) = "Unbound variable " ++ s

instance Error MakeError where
   noMsg  = Default "An error has occurred"
   strMsg = Default

{- 
-- 2. Parse
   let parsed = simpleParse p_makeFile s
-- 3. Lex
   case parsed of
      Right v -> putStrLn $ makeCommand "all" v
      Left err -> putStrLn $ "Parsing error" ++ show err
-}

main :: IO ()
main = do
   putStrLn "Usage:\n\thmake\n\thmake [tag]\n"

   args <- getArgs
   
   let tag = if (length args > 0) then (args !! 0) else "all"
   let filename = "Makefile"
   isFile <- doesFileExist filename
   if isFile
   then 
      makeMain filename tag
   else 
      putStrLn $ filename ++ "does not exist!"

makeMain filename tag = do
   content <- readFile filename
   putStrLn "Content:"
   print content 

   -- 1. Read/Parse
   parsed <- runIOThrows $ parseString content 
   -- parsed <- runIOThrows $ parseString makefile1
   putStrLn "Parsed:"
   putStrLn $ show parsed
   
   let processed = tProc parsed
   case (checkProc processed) of 
      Left err -> do
         putStrLn err
         exitFailure
      Right _ -> do
         putStrLn "Processed:"
         print processed
         let cmds = makeCommand tag processed
         case cmds of 
            Nothing -> exitWith ExitSuccess
            Just v  -> do
               result <- runCommands (map parseCmd v)
               case result of 
                  Left err -> exitFailure 
                  Right _  -> exitWith ExitSuccess  


runCommands :: [[String]] -> IO (Either String String)
runCommands ss = 
   foldM p (Right "") ss
   where 
      p r s = 
         case r of
            Left err -> return $ Left err
            Right _ -> runCmd s

runCmd :: [String] -> IO (Either String String)
runCmd s = 
   if not (null s)
   then
      putStrLn (unwords s) >> 
      rawSystem (head s) (tail s) >>= \r ->
      case r of 
         ExitSuccess -> return $ Right "Success"
         ExitFailure n -> return $ Left $ "Fail with " ++ show n ++ " Cmd:" ++ show s
    else 
      return $ Left "Command is empty"

simpleParse p input = 
   parse p "make" input 

p_makeFile = 
   parseExpr `endBy` spaces 

parseCmd :: String -> [String]
parseCmd input =
   case (simpleParse (p_cmd []) input) of 
      Left err -> []
      Right v  -> v

{-
readOrThrow :: Parser a -> String -> MakeVal
readOrThrow parser input = case parse parser "make" input of
        Left err  -> Comment ("No match: " ++ show err)
        Right val -> val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
-}

type ThrowsError = Either MakeError
type IOThrowsError = ErrorT MakeError IO

readOrThrowE :: Parser a -> String -> ThrowsError a
readOrThrowE parser input = 
   case parse parser "make" input of
      Left err -> throwError $ Parser err
      Right val -> return val

readExprE :: String -> ThrowsError MakeVal
readExprE = readOrThrowE parseExpr
readExprListE = readOrThrowE (endBy parseExpr spaces)

parseString str = 
   (liftThrows . readExprListE $ str)

trapError action = catchError action (return . show)
trapErrorDirect action = catchError action (return)

extractValue :: ThrowsError [MakeVal] -> [MakeVal]
extractValue (Left err) = [Comment $ "Error in Parsing: " ++ show err]
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError [MakeVal] -> IO [MakeVal]
runIOThrows action = runErrorT action >>= return . extractValue

--
-- Parser
--
p_finddollar = do
   char '$'
   char '('
   v <- many (noneOf "})")
   char ')'
   return $ Left $ show $ UnboundVar ("$(" ++ v ++ ")") 

p_findtoken :: CharParser () (Either String String)
p_findtoken = do
   skipMany (noneOf "$")
   (try p_finddollar <|> return (Right ""))

p_cmd :: [String] -> CharParser () [String]
p_cmd v0 = 
   (spaces >> p_cmd v0)
   <|> do
      v <- many1 (noneOf " \t\n\r")
      p_cmd (v0 ++ [v])
   <|> return v0 

parseExpr :: CharParser () MakeVal
parseExpr = parseRuleAndAtom
        <|> parseComment

parseComment :: CharParser () MakeVal
parseComment =  
   (spaces >> parseComment)
   <|> do first <- char '#'
          rest <- many (noneOf "\n\r")
          return $ Comment (first:rest)               

parseRuleAndAtom :: CharParser () MakeVal
parseRuleAndAtom =  
   (spaces >> parseRuleAndAtom)
   <|> do 
      first <- letter <|> underscore
      rest <- many (letter <|> digit <|> underscore <|> dot)
      let n = first:rest
      (try (skipMany spaces >> parseRuleAndAtom2 n) <|> parseRuleAndAtom2 n)

parseRuleAndAtom2 n = do
      sep <- oneOf ":="
      case sep of 
         '=' -> 
            parseAtomVal n ""
         ':' -> 
            (try $ parseRuleDep n "") <|> (parseRuleComm n [] [])

parseAtomVal n val = do
   val2 <- many (noneOf "\n\r")
   if val2 /= [] && last val2 == '\\'
   then do
      eol
      parseAtomVal n (val ++ (dropLastChar val2))
   else
      return $ Atom n (sepSpaces $ val ++ val2)

parseRuleDep n val = do
   val2  <- many (noneOf "\n\r")
   if val2 /= [] && last val2 == '\\'
   then do
      eol
      parseRuleDep n (val ++ (dropLastChar val2))
   else
      parseRuleComm n (sepSpaces $ val ++ val2) []

parseRuleComm n val comm = do
   eol
   char '\t'
   comm2 <- parseRuleCommNextLine [] 
   let comm_set = comm ++ [comm2]
   (try (parseRuleComm n val comm_set)) <|> (return $ Rule n val comm_set)

parseRuleCommNextLine comm = do
   comm2 <- many (noneOf "\n\r")
   if comm2 /= [] && last comm2 == '\\'
   then do
      eol
      parseRuleCommNextLine (comm ++ (dropLastChar comm2))
   else
      return $ comm ++ comm2

--
-- Parser Helper
--

deliner = strRep [("\\\n", ""), ("\\\n\r", ""), ("\\\r", ""), ("\\\r\n", "")]

eol = string "\n" <|> string "\n\r"

spaces :: Parser ()
spaces = skipMany1 space

dot :: Parser Char
dot = oneOf "."

underscore :: Parser Char
underscore = oneOf "_"

--
-- Generators 
--

tAllNames :: [MakeVal] -> [String]
tAllNames ts = 
   foldr procElem [] ts
   where procElem (Atom n _) x = n : x
         procElem (Rule n _ _) x = n : x
         procElem (Comment n) x = x

tPruneComment :: [MakeVal] -> [MakeVal]
tPruneComment ts =
   foldr proc' [] ts
   where proc' v@(Atom n _) x = v : x
         proc' v@(Rule n _ _) x = v : x
         proc' _ x = x

tRepList :: [MakeVal] -> [(String, String)]
tRepList ts =
   foldr proc' [] ts
   where proc' (Atom n m) x = ("${" ++ n ++ "}", unwords m) 
                            : ("$(" ++ n ++ ")", unwords m)
                            : x
         proc' _ x = x

tTrans :: [MakeVal] -> [MakeVal]
tTrans ts =
   map proc' ts
   where proc' (Atom n m) = Atom n (map repl m)
         proc' (Rule n d c) = Rule n (repld' d) (map repl c)
         proc' (Comment s) = Comment s
         repl = strRep (tRepList ts)
         repld' [] = []
         repld' [d] = words (repl d)
         repld' (d:ds) = repld' [d] ++ repld' ds

tOnlyRules :: [MakeVal] -> [MakeVal]
tOnlyRules ts =  
   foldr proc' [] ts
   where proc' v@(Rule n _ _) x = v : x
         proc' _ x = x

ruleName    (Rule n d c) = n
ruleDep     (Rule n d c) = d
ruleCommand (Rule n d c) = c

linearSortDep :: [MakeVal] -> [MakeVal]
linearSortDep [] = []
linearSortDep [x] = [x]
linearSortDep (x:xs) = insert' (linearSortDep xs)
   where insert' []     = [x]
         insert' (y:ys) | ruleName x `elem` ruleDep y  = x : y : ys
                        | otherwise = y : insert' ys
tProc = 
   linearSortDep . tOnlyRules . tTrans . tPruneComment

addEmptyOneIfNull s = 
   if null s
   then [""]
   else s

checkProc :: [MakeVal] -> Either String String
checkProc ts =
   let str = unlines $ map (unwords . ruleCommand) ts in
   case (simpleParse p_findtoken str) of 
      Left e          -> Left $ show e
      Right (Left e)  -> Left e
      Right (Right v) -> do
         
         Right v

makeCommand :: [Char] -> [MakeVal] -> Maybe [String]
makeCommand ""      ts = makeCommand "all" ts
makeCommand "all"   ts = Just $ concat (map ruleCommand $ filter (\x -> ruleName x /= "clean") ts)
makeCommand "clean" ts = Just $ concat (map ruleCommand $ filter (\x -> ruleName x == "clean") ts)
makeCommand _ _        = Nothing

--
-- String Helper
--

dropLastChar val = reverse (drop 1 (reverse val))

sepSpaces ws = wordsWhen (\x -> if x == ' ' || x == '\t' then True else False) ws

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> [""] 
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

trim w = reverse (dropWhileSpaces (dropWhileSpaces $ reverse w))
   where dropWhileSpaces = dropWhile (\x -> if x == ' ' || x == '\t' then True else False)

replace :: String -> (String,String) -> String
replace s (a,b) = let [ss,aa,bb] = [T.pack x | x <- [s,a,b]] in
  T.unpack $ T.replace aa bb ss

strRep = flip $ foldl' replace

-- Test strRep  "${CC} abc" "gcc ${CC}"

delimiterSplit      :: String -> Char -> [String]
delimiterSplit "" _ =  []
delimiterSplit s  c =  let (r,rs) = break (== c) s
                       in r : delimiterSplit rs c

delimiterJoin       :: [a] -> a -> [a]
delimiterJoin       =  flip intersperse

--
-- Sample Data
--

makefile1 = "OBJS=file1.o\tfile2.o\nCC=gcc\nCFLAGS=-Wall\t-O\t-g\n\nhelloworld:$(OBJS)\n\t$(CC)\t$(OBJS)\t-o\thelloworld\n\nfile1.o:file1.c\tfile2.h\n\t$(CC)\t$(CFLAGS)\t-c\tfile1.c\t-o\tfile1.o\n\nfile2.o:file2.c\tfile2.h\n\t$(CC)\t$(CFLAGS)\t-c\tfile2.c\t-o\tfile2.o\n\nclean:\n\trm\t-rf\t*.o\thelloworld\n"

makeTree1 = [Atom "OBJS" ["file1.o","file2.o"],Atom "CC" ["gcc"],Atom "CFLAGS" ["-Wall","-O","-g"],Comment "# 1:",Comment "# all1: ${OBJS}",Comment "# all: ${OBJS}",Rule "helloworld" ["$(OBJS)"] ["$(CC)\t$(OBJS)\t-o\thelloworld"],Rule "file1.o" ["file1.c","file2.h"] ["$(CC)\t$(CFLAGS)\t-c\tfile1.c\t-o\tfile1.o"],Rule "file2.o" ["file2.c","file2.h"] ["$(CC)\t$(CFLAGS)\t-c\tfile2.c\t-o\tfile2.o"],Rule "clean" [] ["rm\t-rf\t*.o\thelloworld"]]

makeTree12 = [Rule "clean" [] ["rm\t-rf\t*.o\thelloworld","rm\t-rf\t*.o\thelloworld"],Rule "file2.o" ["file2.c","file2.h"] ["gcc\t-Wall -O -g\t-c\tfile2.c\t-o\tfile2.o","gcc\t-Wall -O -g\t-c\tfile2.c\t-o\tfile2.o"],Rule "file1.o" ["file1.c","file2.h"] ["gcc\t-Wall -O -g\t-c\tfile1.c\t-o\tfile1.o","gcc\t-Wall -O -g\t-c\tfile1.c\t-o\tfile1.o"],Rule "helloworld" ["file1.o","file2.o"] ["gcc\tfile1.o file2.o\t-o\thelloworld","gcc\tfile1.o file2.o\t-o\thelloworld"]]
