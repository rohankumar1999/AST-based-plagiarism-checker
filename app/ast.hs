module Ast (
  Expression(..),
  Binop(..),
  Value(..),
  showAst,
) where
  

import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import Data.Hashable

-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String


data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Lst Expression
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
  | FunctionDecl Variable Expression Expression -- def fun(arg1,arg2=3) <body>
  -- | FunctionCall Variable Expression
  deriving (Show)
type List = [Int]

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Show)

op_hashes x = 
    case x of
        "+" -> 8
        "-" -> 9
        "*" -> 10
        "/" -> 11
        ">" -> 12
        "<" -> 13
        ">=" -> 14
        "<=" -> 15
        ":=" -> 16
data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)

hashes = []

hashVal :: Int -> Int
hashVal x = hash $show x

fileP :: GenParser Char st (Expression, List)
fileP = do
  (prog, hash_prog) <- exprP
  eof
  return (prog, hash_prog)

exprP = do
  (e, hash_e) <- exprP'
  rest <- optionMaybe restSeqP
  return (case rest of
    Nothing   -> (e, hash_e)
    Just (e', hash_e') -> (Sequence e e', hash_e++hash_e'))

-- Expressions are divided into terms and expressions for the sake of
-- parsing.  Note that binary operators **DO NOT** follow the expected
-- presidence rules.

exprP' = do
  spaces
  (t, hash_t) <- termP
  spaces
  rest <- optionMaybe restP
  spaces
  return (case rest of
    Nothing   -> (t, hash_t)
    Just (":=", hash_assign, t', hash_t') -> (case t of
      Var varName -> (Assign varName t', hash_t++[hash_assign]++hash_t'++[(hash_assign::Int)+ last hash_t + last hash_t'])
      _           -> error "Expected var")
    Just ("-", hash_op, t', hash_t') -> (Op (transOp "-") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just ("/", hash_op, t', hash_t') -> (Op (transOp "/") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just (">", hash_op, t', hash_t') -> (Op (transOp ">") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just ("<", hash_op, t', hash_t') -> (Op (transOp "<") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just (">=", hash_op, t', hash_t') -> (Op (transOp ">=") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just ("<=", hash_op, t', hash_t') -> (Op (transOp "<=") t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + 2 * last hash_t'])
    Just (op, hash_op, t', hash_t') -> (Op (transOp op) t t', [hash_op]++hash_t++hash_t' ++ [(hash_op::Int) + last hash_t + last hash_t']))

restSeqP = do
  char ';'
  exprP

transOp s = case s of
  "+"  -> Plus
  "-"  -> Minus
  "*"  -> Times
  "/"  -> Divide
  ">=" -> Ge
  ">"  -> Gt
  "<=" -> Le
  "<"  -> Lt
  o    -> error $ "Unexpected operator " ++ o

-- Some string, followed by an expression
restP = do
  ch <- string "+"
    <|> string "-"
    <|> string "*"
    <|> string "/"
    <|> try (string "<=")
    <|> string "<"
    <|> try (string ">=")
    <|> string ">"
    <|> string ":=" -- not really a binary operator, but it fits in nicely here.
    <?> "binary operator"
  (e, hash_e) <- exprP'
  return (ch, op_hashes ch, e, hash_e)

-- All terms can be distinguished by looking at the first character
termP = valP
    <|> listP
    <|> funP
    <|> ifP
    <|> whileP
    <|> parenP
    <|> varP
    <?> "value, variable, 'def', 'if', 'while', or '('"


valP = do
  (v, hash_child) <- boolP <|> numberP
  return $ (Val v, hash_child)

boolP = do
  bStr <- string "true" <|> string "false" <|> string "skip"
  return $ case bStr of
    "true" -> (BoolVal True, [hashVal (1::Int)])
    "false" -> (BoolVal False, [hashVal (1::Int)])
    "skip" -> (BoolVal False, [hashVal (1::Int)])

numberP = do
  n <- many1 digit
  return $ (IntVal (read n), [read n])

listP = do
  string "["
  (elements, hash_elements) <- list_elements
  string "]"
  return (Lst elements, hash_elements)

list_elements = do
  (elem, hash_elem) <-  list_elem
  rest <- optionMaybe rest_elems
  return (case rest of
    Nothing   -> (elem, hash_elem)
    Just (e', hash_e') -> (Sequence elem e', hash_elem++hash_e'))

list_elem = do
  (elem, hash_elem) <- valP
  return (elem, hash_elem)

rest_elems = do
  char ','
  spaces
  list_elements

varP = do
  firstChar <- letter
  restChars <- many (alphaNum <|> char '_')
  return $ (Var (firstChar : restChars), [hashVal (3::Int)])

ifP = do
  spaces
  string "if"
  (e1, hash_e1) <- exprP 
  string "then"
  (e2, hash_e2) <- exprP 
  string "else"
  (e3, hash_e3) <- exprP 
  string "endif"
  return $ (If e1 e2 e3,  hash_e1 ++ hash_e2 ++ [hashVal (4::Int)] ++ hash_e3 ++ [hashVal (4::Int)+ last hash_e1+ last hash_e2+ last hash_e3])

funP = do
  spaces
  string "def"
  spaces
  (fname, hash_fname) <- varP
  string "("
  (args, hash_args) <- f_args
  string ")"
  string ":"
  (body, hash_body) <- exprP
  string "enddef"
  return( case fname of
    Var varName -> (FunctionDecl varName args body, hash_fname ++ hash_args ++ hash_body ++ [hashVal (0::Int)] ++ [hashVal (0::Int) + last hash_fname + last hash_args + last hash_body])
    _ -> error "expected variable")



f_args = do
  (args, hash_args) <-  f_arg
  rest <- optionMaybe rest_args
  return (case rest of
    Nothing   -> (args, hash_args)
    Just (e', hash_e') -> (Sequence args e', hash_args++hash_e'))

f_arg = do
  (arg, hash_arg) <- varP
  default_assgn <- optionMaybe restP
  return $ case default_assgn of
    Nothing -> (arg, hash_arg)
    Just (":=", hash_assign, t', hash_t') -> case arg of
      Var argument ->    (Assign argument t', hash_arg++[hash_assign]++hash_t'++[(hash_assign::Int)+ last hash_arg + last hash_t'])
      _ -> error "error"
    _           -> error "Expected assignment"


rest_args = do
  char ','
  spaces
  f_args

whileP = do
  spaces
  string "while"
  (e1, hash_e1)<- exprP
  string "do"
  (e2, hash_e2) <- exprP
  string "endwhile"
  return $ (While e1 e2, hash_e1 ++ [hashVal (5::Int)] ++  hash_e2 ++ [hashVal (5::Int) + last hash_e1 + last hash_e2])

-- An expression in parens, e.g. (9-5)*2
parenP = do
  string "("
  spaces
  (e1, hash_e1) <- exprP
  spaces
  string ")"
  rest <- optionMaybe restP
  spaces
  return $ case rest of
    Nothing -> (e1, hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [last hash_e1+ hashVal 6 + hashVal 7])
    Just ("-", hash_op, e', hash_e') ->  (Op (transOp "-") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just ("/", hash_op, e', hash_e') ->  (Op (transOp "/") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just (">=", hash_op, e', hash_e') ->  (Op (transOp ">=") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just (">", hash_op, e', hash_e') ->  (Op (transOp ">") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just ("<", hash_op, e', hash_e') ->  (Op (transOp "<") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just ("<=", hash_op, e', hash_e') ->  (Op (transOp "<=") e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + 2 * last hash_e1 + last hash_e' + hashVal 6 + hashVal 7])
    Just (op, hash_op, e', hash_e') -> (Op (transOp op) e1 e', hash_e1 ++ [hashVal 6] ++ [hashVal 7] ++ [hash_op] ++ hash_e' ++ [hash_op + last hash_e1 + last hash_e' + hashVal 6 + hashVal 7]) -- assuming - and / are symmetrical. need to change



showAst fileName = do
  p <- parseFromFile fileP fileName
  return (case p of
        Left err -> []
        Right (expr, list) -> list)