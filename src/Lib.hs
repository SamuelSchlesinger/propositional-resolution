module Lib where

import Data.Set (Set)
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Term = T String | F String
 deriving (Eq, Ord, Show)

unterm (T x) = x
unterm (F x) = x

type Clause = Set Term

appearsPositively :: String -> Clause -> Bool
appearsPositively x c = Set.foldr (\term b -> case term of 
  T y -> x == y || b
  F y -> b) False c

appearsNegatively :: String -> Clause -> Bool
appearsNegatively x c = Set.foldr (\term b -> case term of
  F y -> x == y || b
  T y -> b) False c

clauseContainsVar :: String -> Clause -> Bool
clauseContainsVar x = Set.member x . Set.map unterm

type CNF = Set Clause

varsClause :: Clause -> Set String
varsClause = Set.map unterm

vars :: CNF -> Set String
vars = Set.foldr (\clause vs -> varsClause clause `Set.union` vs) Set.empty

resolve :: CNF -> CNF
resolve cnf = if cnf == cnf' then cnf else resolve cnf' where
  cnf' = resolveStep cnf

pairs :: Ord a => Set a -> Set (a, a)
pairs set = Set.foldr (\x pairs -> Set.map (\y-> (x, y)) set `Set.union` pairs) Set.empty set

resVar :: String -> Clause -> Clause -> Maybe Clause
resVar v c1 c2 = if v `appearsNegatively` c1 && v `appearsPositively` c2 then
      Just ((Set.delete (F v) c1)  `Set.union` (Set.delete (T v) c2))
    else if v `appearsPositively` c1 && v `appearsNegatively` c2 then
      Just ((Set.delete (T v) c1) `Set.union` (Set.delete (F v) c2))
    else Nothing

unions :: Ord a => Set (Set a) -> Set a
unions = foldr Set.union Set.empty

resolveStep :: CNF -> CNF
resolveStep cnf = Set.union nextThing cnf where
  vsets :: Set (String, CNF)
  vsets = Set.map (\v -> (v, Set.filter (clauseContainsVar v) cnf)) vs
  nextThing = unions $ Set.map (\(v, vset) -> Set.fromList $ do 
    (c1, c2) <- Set.toList $ pairs vset
    maybeToList (resVar v c1 c2)
    ) vsets
  vs = vars cnf

cnf :: [[Term]] -> CNF
cnf = Set.fromList . map Set.fromList

cs = ["C_{" ++ show x ++ "}" | x <- [1..]]

latexifyClause :: [Term] -> String
latexifyClause (T x : []) = x
latexifyClause (F x : []) = "\\neg " ++ x
latexifyClause (T x : ts) = x ++ ", " ++ latexifyClause ts
latexifyClause (F x : ts) = "\\neg " ++ x ++ ", " ++ latexifyClause ts
latexifyClause [] = ""

latexify :: CNF -> String
latexify cnf = unlines $ zipWith (\x y -> "\\["++ x ++ " = \\{" ++ y ++ "\\} \\]") cs clauses where
  clauses :: [String]
  clauses = map latexifyClause $ map Set.toList (Set.toList cnf)