module DataTypes where

import Text.ParserCombinators.Parsec
-- We represent variables as strings.
type Variable = String

--We also represent error messages as strings.
type ErrorMsg = String

-- Used in storing hash values of subtree rooted at a node
type List = [Int]

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Lst Expression                          -- [Val]
  | Assign Variable Expression              -- x = e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3 endif
  | While Expression Expression             -- while e1 do e2 endwhile
  | FunctionDecl Variable Expression Expression -- def fun(arg1,arg2=3) <body> enddef
  | FunctionSignature Variable Expression   -- func(arg1,arg2=3)
  | FunctionArgs Expression                 -- (arg1, ar2=4)
  | ClassDecl Variable Expression           -- class ClassName body endclass
  | Return Expression                       -- return var|val|list
  deriving (Show)


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

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show)