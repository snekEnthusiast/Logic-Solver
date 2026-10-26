module Parsing
  ( Term(..)
  , AtomicF(..)
  , Clause(..)
  , Program
  , Goals
  , Parser(..)
  , parse
  , item
  , sat
  , char
  , string
  , upper
  , lower
  , alphanum
  , vname
  , fname
  , term
  , predicate
  , predicates
  , clause
  , program
  , clean
  , solutionE
  ) where

import Control.Applicative ( Alternative(..) )
import Data.Char ( isAlphaNum, isLower, isUpper, isSpace )
import Data.Maybe ( isJust )

-- AST

data Term = App String [Term] | Var String deriving (Show,Eq,Ord)
data AtomicF = Pred String [Term] deriving (Show)
data Clause = Clause [AtomicF] [AtomicF] deriving (Show)
type Program = [Clause]
type Goals = [AtomicF]

-- Parser

newtype Parser a = Parser (String -> Maybe (a,String))

instance Functor Parser where
   fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = Parser (\inp -> case parse p inp of
                        Nothing      -> Nothing
                        Just (v,out) -> Just (g v, out))

instance Applicative Parser where
   pure :: a -> Parser a
   pure v = Parser (\inp -> Just (v,inp))

   (<*>) :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = Parser (\inp -> case parse pg inp of
                        Nothing      -> Nothing
                        Just (g,out) -> parse (fmap g px) out)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\inp -> case parse p inp of
                        Nothing      -> Nothing
                        Just (v,out) -> parse (f v) out)

instance Alternative Parser where
   empty :: Parser a
   empty = Parser (const Nothing)

   (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = Parser (\inp -> case parse p inp of
                           Nothing      -> parse q inp
                           Just (v,out) -> Just (v,out))

parse :: Parser a -> String -> Maybe (a,String)
parse (Parser p) = p

item :: Parser Char
item = Parser (\inp -> case inp of
                ""     -> Nothing
                (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

upper :: Parser Char
upper = sat isUpper

lower :: Parser Char
lower = sat isLower

alphanum :: Parser Char
alphanum = sat isAlphaNum

vname :: Parser Term
vname = do
    f <- upper
    r <- many alphanum
    return (Var (f:r))

fname :: Parser Term
fname = do
    f <- lower
    r <- many alphanum
    return (App (f:r) [])

getTName :: Term -> String
getTName (Var name) = name
getTName (App name _) = name

term :: Parser Term
term =  do
        name <- fname
        char '('
        fstTerm <- term
        remTerm <- many (do
                         char ','
                         term)
        char ')'
        return (App (getTName name) (fstTerm:remTerm))
    <|>
        fname
    <|>
        vname

predicate :: Parser AtomicF
predicate = do
    name <- fname
    char '('
    fstTerm <- term
    remTerm <- many (do
                     char ','
                     term)
    char ')'
    return (Pred (getTName name) (fstTerm:remTerm))

predicates :: Parser [AtomicF]
predicates = do
    fst <- predicate
    rest <- many (do
                  char ','
                  predicate)
    return (fst:rest)

clause :: Parser Clause
clause = do
        fst <- predicate
        string ":-"
        snd <- predicates
        char '.'
        return (Clause [fst] snd)
    <|> do
        string ":-"
        snd <- predicates
        char '.'
        return (Clause [] snd)
    <|> do
        fst <- predicate
        char '.'
        return (Clause [fst] [])

program :: Parser Program
program = some clause

-- Cleaner and solver entry

clean :: String -> Maybe String
clean t = if isJust res then Just (filter (not . isSpace) (fromJust res)) else Nothing
    where
    res = helper t
    helper ""            = Just ""
    helper ('%':xs)      = helperInSLC xs
    helper ('/':'*':xs)  = helperInMLC xs
    helper (x:xs)        = if isJust res then Just (x:fromJust res) else Nothing where res = helper xs
    helperInSLC ""        = Just ""
    helperInSLC ('\n':xs) = helper xs
    helperInSLC (x:xs)    = helperInSLC xs
    helperInMLC ""           = Nothing
    helperInMLC ('*':'/':xs) = helper xs
    helperInMLC (x:xs)       = helperInMLC xs

solutionE :: String -> Maybe Program
solutionE s = if isJust cl && isJust res && rest == "" then Just val else Nothing
    where
    cl = clean s
    res = parse program (fromJust cl)
    (val,rest) = fromJust res

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
