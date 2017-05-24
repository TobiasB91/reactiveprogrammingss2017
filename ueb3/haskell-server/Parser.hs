module Parser (parseFormula, Expr (..), Op(..)) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)
import Data.List 

data Expr = Cell String | 
  Const Integer | 
  Binary Op Expr Expr 
  deriving Show 

data Op = Plus | 
  Minus | 
  Multiply | 
  Division 
  deriving Show

lexer = makeTokenParser javaStyle

expr = buildExpressionParser table term 

term =  parens lexer expr 
  <|> (Const <$> natural lexer)
  <|> reduce
  <|> cell

cell = do 
  col <- oneOf ['A'..'Z'] 
  row <- natural lexer 
  return . Cell $ col : show row

reduce = do
  string "REDUCE"
  parens lexer $ do
    Cell start <- cell 
    colon lexer
    Cell end <- cell
    comma lexer
    op <- operand
    let start_col = head start
    let end_col = head end
    let start_row = read . tail $ start :: Integer
    let end_row = read. tail $ end :: Integer
    let range = do 
          c <- [start_col..end_col] 
          r <- [start_row..end_row]
          return . Cell $ c:(show r)
    return $ foldl' (Binary op) (head range) (tail range)

operand = (reservedOp lexer "*" >> return Multiply)
  <|> (reservedOp lexer "/" >> return Division)
  <|> (reservedOp lexer "+" >> return Plus)
  <|> (reservedOp lexer "-" >> return Minus)
  

table = [ [Infix (reservedOp lexer "*" >> return (Binary Multiply)) AssocLeft, 
    Infix (reservedOp lexer "/" >> return (Binary Division)) AssocLeft]
    ,[Infix (reservedOp lexer "+" >> return (Binary Plus))  AssocLeft, 
    Infix (reservedOp lexer "-" >> return (Binary Minus)) AssocLeft] ]

parseFormula :: String -> Either ParseError Expr 
parseFormula = parse expr ""
