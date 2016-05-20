check_hukumen :: [Int] -> [String] -> [String]
check_hukumen (a:b:d:e:h:s:t:r:[]) z =
  if d/=0 && s/=0 && debt + star == death
  then expr:z else z
  where
    debt  =             d * 1000 + e * 100 + b * 10 + t
    star  =             s * 1000 + t * 100 + a * 10 + r
    death = d * 10000 + e * 1000 + a * 100 + t * 10 + h
    expr  = show debt ++ "+" ++ show star ++ "=" ++ show death

check_hukumen' :: [Int] -> [String] -> [String]
check_hukumen' (a:b:e:h:s:t:r:[]) z =
  if debt + star == death
  then expr:z else z
  where
    debt  =             d * 1000 + e * 100 + b * 10 + t
    star  =             s * 1000 + t * 100 + a * 10 + r
    death = d * 10000 + e * 1000 + a * 100 + t * 10 + h
    expr  = show debt ++ "+" ++ show star ++ "=" ++ show death
    d = 1

hukumen_solver :: [String]
hukumen_solver = 
  foldr check_hukumen [] (permutation 8 [0,1,2,3,4,5,6,7,8,9])

hukumen_solver' :: [String]
hukumen_solver' = 
  foldr check_hukumen' [] (permutation 7 [0,2,3,4,5,6,7,8,9])


main = print hukumen_solver'


select :: [a] -> [(a, [a])]
select [x]    = [(x, [])]
select (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (select xs)

permutation :: Int -> [a] -> [[a]]
permutation 0 _  = [[]]
permutation n xs =
  concatMap (\(y, ys) -> map (y:) (permutation (n - 1) ys)) $ select xs
