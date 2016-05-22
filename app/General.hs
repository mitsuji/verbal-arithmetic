{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Prelude hiding ((+),(-),(*),(==))
import qualified Prelude as P
import Data.String (IsString(fromString))
import Data.List (nub,union)

data VExp = Val [Char]
          | Add VExp VExp

instance IsString VExp where
  fromString xs = Val xs

(+) = Add


calc :: VExp -> VExp -> [[(Char,Int)]]
calc e1 e2 =
  let
    c = nub $ chars e1 `union` chars e2
  in
   filter (matchG e1 e2) $ map (zip c) $ permutation (length c) [0..9]

(==) = calc


matchG :: VExp -> VExp -> [(Char,Int)] -> Bool
matchG e1 e2 c =
  let
    fc = nub $ fchars e1 `union` fchars e2
  in
   (and $ map f fc ) && eToN e1 c P.== eToN e2 c
  where
    f x =
      let Just y = lookup x c
      in y /= 0
      

chars :: VExp -> [Char]
chars (Val xs) = xs
chars (Add e1 e2) = chars e1 `union` chars e2

fchars :: VExp -> [Char]
fchars (Val (x:xs)) = [x]
fchars (Add e1 e2) = fchars e1 `union` fchars e2



eToN :: VExp -> [(Char,Int)] -> Int
eToN (Add e1 e2) c = eToN e1 c P.+ eToN e2 c
eToN (Val xs) c = intListToInt $ map f xs
  where
    f x =
      let Just y = lookup x c
      in y
      

intListToInt :: [Int] -> Int
intListToInt xs = f 1 $ reverse xs
  where
    f :: Int -> [Int] -> Int
    f _ [] = 0
    f d (x:xs) = (d P.* x) P.+ (f (d P.* 10) xs)



main = do
  print $ "debt" + "star" == "death"
--  print $ "send" + "more" == "money"




select :: [a] -> [(a, [a])]
select [x]    = [(x, [])]
select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

permutation :: Int -> [a] -> [[a]]
permutation 0 _  = [[]]
permutation n xs =
  concatMap (\(y, ys) -> map (y:) (permutation (n P.- 1) ys)) $ select xs
