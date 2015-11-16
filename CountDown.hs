module Main where

import Data.List (delete)
import Data.Set (fromList, toList)

data Operator = Divide | Multiply | Add | Subtract deriving (Eq, Enum, Ord)
data Expr     = Literal Int | Expr Operator Expr Expr deriving (Eq, Ord)

class OperatorRules a where
  isValid :: a -> Bool

instance Show Expr where
  show (Literal x)      = show x
  show (Expr op x y)    = "(" ++ show x ++ operator ++ show y ++ ")" 
    where operator = case op of
                       Divide   -> " / "
                       Multiply -> " * "
                       Add      -> " + "
                       Subtract -> " - "

instance OperatorRules Expr where
  isValid (Expr Divide x y)             = eval y > 1 && rem (eval x) (eval y) == 0
  isValid (Expr Multiply _ (Literal 1)) = False
  isValid (Expr Subtract x y)           = x > y
  isValid _                             = True

-- Evaluate an Expression
eval :: Expr -> Int
eval (Literal x)         = x
eval (Expr Multiply x y) = (eval x) * (eval y)
eval (Expr Divide x y)   = (eval x) `div` (eval y)
eval (Expr Add x y)      = (eval x) + (eval y)
eval (Expr Subtract x y) = (eval x) - (eval y)

-- Find solution(s) - get all valid combinations of literals and operators and search for target
solve :: Int -> [Int] -> [Expr]
solve target xs = (toList . fromList) (combinations xs >>= search)
  where combinations :: [Int] -> [Expr]
        combinations []      = []
        combinations numbers = map head $ combinations' (map Literal numbers)
          where combinations' :: [Expr] -> [[Expr]]
                combinations' [] = []
                combinations' (x:[]) = [[x]]
                combinations' xs = (concat [ reduce op (pairs xs) | op <- [Divide ..] ]) >>= combinations'
                  where pairs :: [Expr] -> [((Expr, Expr), [Expr])]
                        pairs []     = []
                        pairs [x]    = []
                        pairs (y:ys) = [ (largestFirst y z, (delete y . delete z) xs) | z <- ys ] ++ pairs ys
                        largestFirst x y = if x >= y then (x,y) else (y,x)
                        reduce op = filter (not . null) . map (reduce' op)
                          where reduce' :: Operator -> ((Expr,Expr), [Expr]) -> [Expr]
                                reduce' op ((x,y), remaining) = if isValid e then e:remaining else []
                                  where e = Expr op x y

        search :: Expr -> [Expr]
        search e = (if eval e == target then [e] else []) ++ case e of
          Expr _ x y -> search x ++ search y
          Literal _  -> []


-- Main
main :: IO ()
main = do let values = [75,50,100,9,7,2]
          let target = 701
          let solutions = solve target values
          putStrLn (show solutions)
