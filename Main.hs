{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Solution (solution) where

import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Parsing (Clause(..), Program, Term(..), solutionE)
import SLDResolution (findVars, readGoal, sDFS, solutionH)

solution :: IO ()
solution = interact (fromMaybe "" . findSolution)

findSolution :: String -> Maybe String
findSolution str = if isJust program && isJust target
                    then Just output
                    else Nothing
    where
    program = solutionE str
    (clauses,goals) = createProgram $ fromJust program
    tree = solutionH clauses $ map readGoal goals
    target = sDFS tree
    initSubs = Subs (map (\t -> (t,t)) (initVars goals))
    subs = joinSubs initSubs $ fromJust target
    output = prepareOutput subs ""

createProgram :: Program -> (Program,Program)
createProgram [] = ([],[])
createProgram (c:cs) | isFact c || isRule c = (c:p,g) where (p,g) = createProgram cs
createProgram (c:cs) = (p,c:g) where (p,g) = createProgram cs

initVars :: [Clause] -> [Term]
initVars cs = nub $ concat vars
    where
    vars = map findVars cs

isFact :: Clause -> Bool
isFact (Clause [_] []) = True
isFact _ = False

isRule :: Clause -> Bool
isRule (Clause [_] _) = True
isRule _ = False
