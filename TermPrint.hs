module TermPrint (solutionD) where

import Parsing (Term(..))

solutionD :: Term -> ShowS
solutionD (Var var) = showString var
solutionD (App name []) = showString name
solutionD (App name lst) = showString name . showString "(" . foldr1 (\a b-> a . showString "," . b) (map solutionD lst) . showString ")"
