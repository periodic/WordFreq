{-# LANGUAGE OverloadedStrings #-}
module Main where

{- | This program does simple word frequency counting and prints the results as
 - a histogram.  It uses a state monad to keep track of the state as it
 - iterates over files and words, and gathers some statistics as it does so.
 - The state monad may be a little bit over overkill, but it helps to reduce
 - the number of function arguments and state that has to be passed between
 - functions.
 -
 - The most important part of the program is `registerWord`.  It is responsible
 - for updating the state.  All other functions either iterate over data or are
 - responsible for IO.
 -}

import System.Environment (getArgs)
import Data.Map (insertWith, empty, toList)

main = getArgs >>= return . head >>= readFile >>= return . foldr (\w m -> insertWith (+) w 1 m) empty . words >>= print . toList
