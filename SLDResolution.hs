module SLDResolution
  ( solutionH
  , makeBranch
  , isRelevant
  , factBranch
  , ruleBranch
  , isFact
  , isRule
  , readFact
  , readGoal
  , readRule
  , unifyFact
  , unifyRule
  , replaceGoal
  , replaceClause
  , flattenFilterVars
  , findVars
  , makeTemps
  , nextTemp
  , isTemp
  , fixStack
  , infect
  , sDFS
  ) where

import Data.List ( nub )
import Data.Maybe ( fromJust, isJust, listToMaybe, mapMaybe )
import Parsing ( AtomicF(..), Clause(..), Goals, Program, Term(..) )
import Unification ( Subs(..), Stack, SLDTree(..), unify )

solutionH :: Program -> Goals -> SLDTree
solutionH p [] = SLDTree (Subs []) [] []
solutionH p g  = makeBranch p (Subs []) g 0

makeBranch :: Program -> Subs -> Goals -> Int -> SLDTree
makeBranch p s [] i     = SLDTree s [] []
makeBranch p s (g:gs) i = SLDTree s (g:gs) (mapMaybe (\c->helper p gs i c g) p)
  where
    helper p gs i c g
        | isFact c = factBranch p gs i c g
        | isRule c = ruleBranch p gs i c g

isRelevant :: Clause -> AtomicF -> Bool
isRelevant (Clause [Pred cName _] _) (Pred gName _) = cName == gName

isFact :: Clause -> Bool
isFact (Clause [_] []) = True
isFact _ = False

readFact :: Clause -> [Term]
readFact (Clause [Pred name l] []) = l

readGoal :: Clause -> AtomicF
readGoal (Clause [] [g]) = g

unifyFact :: Clause -> AtomicF -> Maybe Subs
unifyFact factClause goal = if   isRelevant factClause goal
                            then unify (zip gterms rterms)
                            else Nothing
    where
    (Pred name gterms) = goal
    rterms = readFact factClause

factBranch :: Program -> Goals -> Int -> Clause -> AtomicF -> Maybe SLDTree
factBranch program nextGoals prevHighest clause goal = if isJust unifyRes then
                                                       Just $ makeBranch program jSubs sGoals nmax
                                                       else Nothing
    where
    newVars = findVars clause
    (newTemps,nmax) = makeTemps prevHighest (length newVars)
    swapStack = zip newVars newTemps
    tempclause = replaceClause swapStack clause
    unifyRes = unifyFact tempclause goal
    jSubs = fromJust unifyRes
    (Subs nStack) = jSubs
    sGoals = map (replaceGoal (fixStack nStack)) nextGoals

isRule :: Clause -> Bool
isRule (Clause [_] _) = True
isRule _ = False

readRule :: Clause -> ([Term],[AtomicF])
readRule (Clause [Pred name l] reqs) = (l,reqs)

unifyRule :: Clause -> AtomicF -> Maybe (Subs,Goals)
unifyRule ruleClause goal = if   isRelevant ruleClause goal
                            then infect (unify (zip gterms rterms),newRules)
                            else Nothing
    where (Pred name gterms) = goal
          (rterms, newRules) = readRule ruleClause

nextTemp :: Int -> (Term,Int)
nextTemp prevMax = (Var ("gnn"++show newMax),newMax) where newMax = prevMax+1

isTemp :: Term -> Bool
isTemp (Var ('g':'n':'n':ts)) = True
isTemp _ = False

replaceGoal :: Stack -> AtomicF -> AtomicF
replaceGoal stack (Pred name terms) = Pred name (map (replaceTerm stack stack) terms)

replaceTerm :: Stack -> Stack -> Term -> Term
replaceTerm []         original (App name terms) = App name (map (replaceTerm original original) terms)
replaceTerm []         original term = term
replaceTerm ((l,r):ts) original term| term == l = r
replaceTerm (_:ts)     original term = replaceTerm ts original term

replaceClause :: Stack -> Clause -> Clause
replaceClause stack (Clause al1 al2) = Clause (map (replaceGoal stack) al1) (map (replaceGoal stack) al2)

flattenFilterVars :: [Term] -> [Term]
flattenFilterVars [] = []
flattenFilterVars ((Var name):ts) = term:flattenFilterVars ts where term = Var name
flattenFilterVars ((App n terms):ts) = flattenFilterVars terms ++ flattenFilterVars ts where term = App n terms

findVars :: Clause -> [Term]
findVars (Clause t1 t2) = nub $ flattenFilterVars $ helper t1 ++ helper t2
    where
    att :: AtomicF -> [Term]
    att (Pred _ t) = t
    helper :: [AtomicF] -> [Term]
    helper [] = []
    helper (a:as) = att a ++ helper as

makeTemps :: Int -> Int -> ([Term],Int)
makeTemps pmax 0 = ([],pmax)
makeTemps pmax n = (vname:nvars,fmax)
    where
    (vname,nmax) = nextTemp pmax
    (nvars,fmax) = makeTemps nmax (n-1)

ruleBranch :: Program -> Goals -> Int -> Clause -> AtomicF -> Maybe SLDTree
ruleBranch program nextGoals prevHighest clause goal = if   isJust unifyRes
                                                       then Just $ makeBranch program jSubs sGoals nmax
                                                       else Nothing
    where
    newVars = findVars clause
    (newTemps,nmax) = makeTemps prevHighest (length newVars)
    swapStack = zip newVars newTemps
    tempclause = replaceClause swapStack clause
    unifyRes = unifyRule tempclause goal
    (jSubs,jGoals) = fromJust unifyRes
    (Subs nStack) = jSubs
    sGoals = map (replaceGoal (fixStack nStack)) (jGoals++nextGoals)

fixStack :: Stack -> Stack
fixStack [] = []
fixStack ((App x t, Var y):xs) = (Var y, App x t):fixStack xs
fixStack (x:xs) = x:fixStack xs

infect :: (Maybe a,b) -> Maybe (a,b)
infect (Just x,y) = Just (x,y)
infect (Nothing,_) = Nothing

sDFS :: SLDTree -> Maybe [Subs]
sDFS (SLDTree subs [] []) = Just [subs]
sDFS (SLDTree subs gs []) = Nothing
sDFS (SLDTree subs goals children) = do
    otherSubs <- listToMaybe (mapMaybe sDFS children)
    return (subs : otherSubs)
