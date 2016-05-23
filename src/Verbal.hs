{-# LANGUAGE OverloadedStrings #-}

module Verbal (
   (+)
  ,(==)
  ,findConditions
  ) where

import Prelude hiding ((+),(-),(*),(==))
import qualified Prelude as P
import Data.String (IsString(fromString))
import Data.List (nub,union)

data VExp = Val [Char]
          | Add VExp VExp

data VEqu = Equals VExp VExp
            
(+)  = Add
(==) = Equals
       
instance IsString VExp where
  fromString xs = Val xs


exp1 :: VEqu 
exp1 = Equals (Add (Val "debt") (Val "star")) (Val "death")

exp2 :: VEqu 
exp2 = Val "debt" `Add` Val "star" `Equals` Val "death"

exp3 :: VEqu 
exp3 = Val "debt" + Val "star" == Val "death"

exp4 :: VEqu 
exp4 = "debt" + "star" == "death"





findConditions :: VEqu -> [[(Char,Int)]]
findConditions equ =
  let
    cs = chars equ
  in
   filter (match equ) $ map (zip cs) $ permutation (length cs) [0..9]


match :: VEqu -> [(Char,Int)] -> Bool
match equ xs = (and $ map f $ firstChars equ) && evaluate equ xs
  where
    f x =
      let Just y = lookup x xs
      in  y /= 0
      

chars :: VEqu -> [Char]
chars (Equals exp1 exp2) = nub $ expChars exp1 `union` expChars exp2
  where
    expChars :: VExp -> [Char]
    expChars (Val xs) = xs
    expChars (Add exp1 exp2) = expChars exp1 `union` expChars exp2
    

firstChars :: VEqu -> [Char]
firstChars (Equals exp1 exp2) = nub $ expFirstChars exp1 `union` expFirstChars exp2
  where
    expFirstChars :: VExp -> [Char]
    expFirstChars (Val (x:xs)) = [x]
    expFirstChars (Add exp1 exp2) = expFirstChars exp1 `union` expFirstChars exp2


evaluate :: VEqu -> [(Char,Int)] -> Bool
evaluate (Equals exp1 exp2) xs = expEvaluate exp1 xs P.== expEvaluate exp2 xs 
  where
    expEvaluate :: VExp -> [(Char,Int)] -> Int
    expEvaluate (Add exp1 exp2) xs = expEvaluate exp1 xs P.+ expEvaluate exp2 xs
    expEvaluate (Val cs) xs = listToInt $ map f cs
      where
        f x =
          let Just y = lookup x xs
          in  y
      

listToInt :: [Int] -> Int
listToInt xs = f 1 $ reverse xs
  where
    f :: Int -> [Int] -> Int
    f _ [] = 0
    f d (x:xs) = (d P.* x) P.+ (f (d P.* 10) xs)




select :: [a] -> [(a, [a])]
select [x]    = [(x, [])]
select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

permutation :: Int -> [a] -> [[a]]
permutation 0 _  = [[]]
permutation n xs =
  concatMap (\(y, ys) -> map (y:) (permutation (n P.- 1) ys)) $ select xs
