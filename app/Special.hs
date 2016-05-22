module Main where

matchDeath :: [Int] -> Bool
matchDeath (a:b:d:e:h:s:t:r:[]) =
  d/=0 && s/=0 && debt + star == death
  where
    debt  =             d * 1000 + e * 100 + b * 10 + t
    star  =             s * 1000 + t * 100 + a * 10 + r
    death = d * 10000 + e * 1000 + a * 100 + t * 10 + h
    
matchDeath' :: [Int] -> Bool
matchDeath' (a:b:e:h:s:t:r:[]) =
  debt + star == death
  where
    debt  =             d * 1000 + e * 100 + b * 10 + t
    star  =             s * 1000 + t * 100 + a * 10 + r
    death = d * 10000 + e * 1000 + a * 100 + t * 10 + h
    d = 1
 
filterDeath =
  map (zip "abdehstr") $ filter matchDeath $ permutation 8 [0,1,2,3,4,5,6,7,8,9]
  
filterDeath' =
  map (zip "abehstr")  $ filter matchDeath'  $ permutation 7 [0,2,3,4,5,6,7,8,9]
  

main = print filterDeath
--main = print filterDeath'


select :: [a] -> [(a, [a])]
select [x]    = [(x, [])]
select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

permutation :: Int -> [a] -> [[a]]
permutation 0 _  = [[]]
permutation n xs =
  concatMap (\(y, ys) -> map (y:) (permutation (n - 1) ys)) $ select xs
