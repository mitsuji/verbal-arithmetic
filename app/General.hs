{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Prelude hiding ((+),(-),(*),(==))
import Verbal
import System.Environment (getArgs)
import Data.String (fromString)

-- for ghci
-- :set -XOverloadedStrings

test1 = print $ findConditions $ "debt" + "star" == "death"

test2 = print $ findConditions $ "send" + "more" == "money"

test3 = do
  ex1:ex2:ex3:_ <- getArgs
  print $ findConditions $ fromString ex1 + fromString ex2 == fromString ex3

test4 = print $ findConditions $ "debt + star = death"

test5 = print $ findConditions $ "send + more = money"
  
test6 = do
  equ:_ <- getArgs
  print $ findConditions $ fromString equ

main = test6

