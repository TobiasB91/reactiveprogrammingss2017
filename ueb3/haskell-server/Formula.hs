module Formula where

import Text.ParserCombinators.Parsec

data Expr = Plus Expr Expr   | Minus Expr Expr  
          | Times Expr Expr  | Div   Expr Expr
          | Literal Integer
          deriving Show

parseExpr :: GenParser Char st Expr
parseExpr = parseTerm `chainl1` addOp

addOp :: GenParser Char st (Expr-> Expr-> Expr)
addOp = do {char '+'; return Plus} <|> do { char '-'; return Minus }

parseTerm :: GenParser Char st Expr
parseTerm = parseFactor `chainl1` timesOp

timesOp :: GenParser Char st (Expr-> Expr-> Expr)
timesOp = do {char '*'; return Times} <|> do { char '/'; return Div }

parseFactor :: GenParser Char st Expr
parseFactor =
  between (char '(') (char ')') parseExpr 
  <|> 
  do ms<- many1 digit 
     return $ Literal (read ms)

parse :: String-> Either String Expr
parse input =  
  case Text.ParserCombinators.Parsec.parse Formula.parseExpr "" input of
    Left err-> Left $ show err
    Right ex-> Right ex