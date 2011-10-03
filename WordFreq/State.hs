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

import qualified Data.Text as T

import Data.Map as M (Map, empty, lookup, insertWith, mapWithKey, toList)

import Control.Monad.State (State, execState, get, put, gets)
import Control.Monad (unless)
import Control.Arrow (first)

import WordFreq.Printer.Histogram
import WordFreq.Reader.Text

-- | The internal representation of the Word List
type WordList = Map Word Integer

-- | The internal state of the wordcounter.  Fields are strict to avoid the build-up of thunks during our many iterations.
data WordCountState = WordCountState { wordMap     :: ! WordList
                                     , maxCount    :: ! Integer
                                     , longestWord :: ! Integer
                                     } deriving (Show)

-- | WordCounter monad type alias.
type WordCounter a = State WordCountState a

{- | Registers a word with the counter.  This takes care of everything from
 - incrementing the count to creating a new key to updating the largest count
 - and updating the longest word. -}
registerWord :: Word -> WordCounter ()
registerWord w = do
    st <- get
    let count   = maybe 1 (+ 1) . M.lookup w . wordMap $ st
        maxC    = {-# SCC "maxCount" #-} max count (maxCount st)
        wlen    = fromIntegral . wordLength $ w
        maxL    = {-# SCC "maxLength" #-} max wlen (longestWord st)
        wmap    = {-# SCC "updateMap" #-} insertWith (+) w 1 (wordMap st)
    {-# SCC "updateState" #-} put $ WordCountState wmap maxC maxL

-- | Registers a set of words.
countWords :: [Word] -> WordCounter ()
countWords = mapM_ registerWord

-- | The initial state of the WordCounter
initialState :: WordCountState
initialState = WordCountState M.empty 0 0

-- | Get the frequencies of words by running the word counter over a list of words.
getFrequencies :: [Word] -> WordCountState
getFrequencies words = execState (countWords words) initialState

-- | The main function.  Reads in words, counts them up, then prints a histogram.
main = do
    words                   <- readWords
    let (WordCountState m c l) = getFrequencies words
    printWordList 80 c l . map (first toString) . toList $ m
