module Main where

import Data.List (delete, permutations)

data Op   = Divide | Multiply | Add | Subtract
            deriving (Eq)

data Expr = Literal Int | Expr Op Expr Expr
            deriving (Eq)

instance Show Expr where
  show (Literal x)      = show x
  show (Expr op x y)    = "(" ++ show x ++ operator ++ show y ++ ")" 
    where operator = case op of
                       Divide   -> " / "
                       Multiply -> " * "
                       Add      -> " + "
                       Subtract -> " - "


-- Evaluate an Expression
eval :: Expr -> Int
eval (Literal x)         = x
eval (Expr Multiply x y) = (eval x) * (eval y)
eval (Expr Divide x y)   = (eval x) `div` (eval y)
eval (Expr Add x y)      = (eval x) + (eval y)
eval (Expr Subtract x y) = (eval x) - (eval y)


-- Find solution(s) - if any
solve :: Int -> [Int] -> [Expr]
solve target xs = combinations xs >>= search
  where search :: Expr -> [Expr]
        search e = (if eval e == target then [e] else []) ++ case e of
          Expr _ x y -> search x ++ search y
          Literal _  -> []


-- All combinations
combinations :: [Int] -> [Expr]
combinations [] = []
combinations numbers = map head $ combinations' (map Literal numbers)
  where combinations' :: [Expr] -> [[Expr]]
        combinations' [] = []
        combinations' (x:[]) = [[x]]
        combinations' xs = (concat $ [ filter (not . null) $ map (reduce' op) (pairs xs) | op <- ops ]) >>= combinations'
          where ops = [Multiply, Divide, Add, Subtract]
                reduce' :: Op -> ((Expr,Expr), [Expr]) -> [Expr]
                reduce' op ((x,y), remaining) = if isValid op then (Expr op x y) : remaining else []
                  where isValid :: Op -> Bool
                        isValid op = case op of
                          Divide    -> y' > 1 && rem x' y' == 0
                          Multiply  -> y' /= 1
                          Subtract  -> x' /= y'
                          otherwise -> True
                        x' = eval x
                        y' = eval y


-- For a list of Expr(essions), return every possible pair, along with the remaining Expr's
pairs :: [Expr] -> [((Expr, Expr), [Expr])]
pairs [] = []
pairs (x:[]) = []
pairs xs = pairs' xs
  where pairs' :: [Expr] -> [((Expr, Expr), [Expr])]
        pairs' [] = []
        pairs' (y:ys) = [(ordered (y,z), (delete y . delete z) xs) | z <- ys] ++ pairs' ys
          where ordered :: (Expr,Expr) -> (Expr,Expr)
                ordered (x,y) = if (eval x) >= (eval y) then (x,y) else (y,x)


-- Main
main :: IO ()
main = do let values = [25,100,10,8,3,5]
          let target = 954
          let solution = solve target values
          putStrLn $ show solution
