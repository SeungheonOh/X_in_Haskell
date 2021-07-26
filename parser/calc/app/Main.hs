{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative        hiding (some)
import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char
import Data.Monoid

type Parser = Parsec Void Text

data Expr a
  = Num a
  | Var String
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Pow (Expr a) (Expr a)
  deriving(Show, Eq)

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) empty empty

lexeme = L.lexeme spaceConsumer
pFloat :: RealFloat a => Parser a
pFloat = lexeme L.float
pDecimal :: RealFloat a => Parser a
pDecimal = lexeme L.decimal

pNumber :: RealFloat a => Parser (Expr a)
pNumber = try $ Num . negate <$> (char '-' *> (try pFloat <|> pDecimal))
            <|> Num <$> (try pFloat <|> pDecimal)

pAdd :: RealFloat a => Parser (Expr a)
pAdd = Add <$> (pNumber <* char '+') <*> pExpr

pSub :: RealFloat a => Parser (Expr a)
pSub = Sub <$> (pNumber <* char '-') <*> pExpr

pMul :: RealFloat a => Parser (Expr a)
pMul = Mul <$> (pNumber <* char '*') <*> pExpr

pDiv :: RealFloat a => Parser (Expr a)
pDiv = Div <$> (pNumber <* char '/') <*> pExpr

pPow :: RealFloat a => Parser (Expr a)
pPow = Pow <$> (pNumber <* char '^') <*> pExpr

pPren :: RealFloat a => Parser (Expr a)
pPren = char '(' *> pExpr <* char ')'

pExpr :: RealFloat a => Parser (Expr a)
pExpr = try pPren
    <|> try pAdd
    <|> try pSub
    <|> try pMul
    <|> try pDiv
    <|> try pPow
    <|> try pNumber

eval :: Floating a => Expr a -> a
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a / eval b
eval (Pow a b) = eval a ** eval b
eval (Num a) = a

main :: IO ()
main = forever $ putStr ">" >> getLine
  >>= either print (print . eval) . parse pExpr "" . T.pack
  
{-| 
  Note!
  This parser doesn't fully work, yet.
  Arithmetic order doesn't work as it should be.
-}