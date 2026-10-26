module Unification
  ( Subs(..)
  , Stack
  , SLDTree(..)
  , unify
  , loop
  , step
  , stackrepl
  , replace
  , inTerm
  , joinSubs
  , applySubs
  , subTerm
  , prepareOutput
  ) where

import Data.Map ( Map, findWithDefault, fromList )
import Data.Maybe ( fromJust, isNothing )
import Parsing ( AtomicF(..), Term(..) )
import TermPrint ( solutionD )

newtype Subs = Subs [(Term, Term)] deriving (Show,Eq)
data SLDTree = SLDTree Subs [AtomicF] [SLDTree] deriving (Show)

type Stack = [(Term,Term)]

joinSubs :: Subs -> [Subs] -> Subs
joinSubs = foldl applySubs

applySubs :: Subs -> Subs -> Subs
applySubs (Subs s1) (Subs s2) = Subs (zip l newr)
    where
    (l,r) = unzip s1
    newr  = map (subTerm (fromList s2)) r

subTerm :: Map Term Term -> Term -> Term
subTerm m (Var n)    = findWithDefault term term m where term = Var n
subTerm m (App n ts) = findWithDefault (App n (map (subTerm m) ts)) term m where term = App n ts

prepareOutput :: Subs -> ShowS
prepareOutput (Subs []) = showString ""
prepareOutput (Subs ((l,r):sx)) = solutionD l . showString " = " . solutionD r . showString ".\n" . prepareOutput (Subs sx)

unify :: [(Term,Term)] -> Maybe Subs
unify t = loop t (Subs [])

loop :: [(Term,Term)] -> Subs -> Maybe Subs
loop []     theta = Just theta
loop (t:ts) theta = if   isNothing nstep
                    then Nothing
                    else loop nstack ntheta
    where
    (x,y) = t
    nstep = step x y ts theta
    (nstack,ntheta) = fromJust nstep

step :: Term -> Term -> Stack -> Subs -> Maybe (Stack,Subs)
step (Var x)    termY      remStack (Subs s)|not (Var x `inTerm` termY) = Just (repl remStack,Subs ((Var x,termY):repl s)) where repl = stackrepl (Var x) termY
step termX      (Var y)    remStack (Subs s)|not (Var y `inTerm` termX) = Just (repl remStack,Subs ((Var y,termX):repl s)) where repl = stackrepl (Var y) termX
step (Var x)    (Var y)    remStack (Subs s)|x==y = Just (remStack, Subs s)
step (App x []) (App y []) remStack (Subs s)|x==y = Just (remStack, Subs s)
step (App x lx) (App y ly) remStack (Subs s)|x==y && length lx == length ly = Just (zip lx ly++remStack, Subs s)
step _ _ _  _= Nothing

stackrepl :: Term -> Term -> Stack -> Stack
stackrepl f r = map (\(x,y)->(replace f r x,replace f r y))

replace :: Term -> Term -> Term -> Term
replace find repl (Var n)   = if   Var n == find
                              then repl
                              else Var n
replace find repl (App n l) = if   App n l == find
                              then repl
                              else App n (map (replace find repl) l)

inTerm :: Term -> Term -> Bool
t `inTerm` (Var name) = t == Var name
t `inTerm` (App n l) = t == App n l || any (t `inTerm`) l
