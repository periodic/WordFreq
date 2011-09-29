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

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.ByteString.Lazy as BS (ByteString, readFile)
import Data.Map as M (Map, empty, lookup, insertWith, mapWithKey, toList)
import Text.Printf (printf)

import Control.Monad.State (StateT, liftIO, evalStateT, get, put, gets)
import Control.Monad (unless)

import System.Environment (getArgs)

import WordFreq.Printer

-- | Simple type synonymn to be used when we expect a single word of Text.
type Word = T.Text
-- | The internal representation of the Word List
type WordList = Map Word Integer

-- | The internal state of the wordcounter.
data WordCountState = WordCountState { wordMap     :: ! WordList
                                     , maxCount    :: ! Integer
                                     , longestWord :: ! Integer
                                     } deriving (Show)

-- | WordCounter monad type alias.
type WordCounter a = StateT WordCountState IO a

{- | Registers a word with the counter.  This takes care of everything from
 - incrementing the count to creating a new key to updating the largest count
 - and updating the longest word. -}
registerWord :: Word -> WordCounter ()
registerWord w =
    let nw = normalizeWord w
     in unless (T.null nw) $
        do  st <- get
            let count   = maybe 1 (+ 1) . M.lookup nw . wordMap $ st
                maxC    = max count (maxCount st)
                wlen    = fromIntegral . T.length $ nw
                maxL    = max wlen (longestWord st)
                wmap    = insertWith (+) nw 1 (wordMap st)
            put $ WordCountState wmap maxC maxL
            return ()

-- | Normalizes a word by removing capitalization and punctuation.
normalizeWord :: Word -> Word
normalizeWord = T.filter (`notElem` "*,.-!?<>/'\";:][}{)(@#$%^&`~-_=+") . T.toLower

-- | Gets the words from a file.
getWordsFromFile :: FilePath -> WordCounter [Word]
getWordsFromFile = fmap (T.words . E.decodeUtf8) . liftIO . BS.readFile

-- | Registers a set of words.
countWords :: [Word] -> WordCounter ()
countWords = mapM_ registerWord

-- | The initial state of the WordCounter
initialState :: WordCountState
initialState = WordCountState M.empty 0 0

-- | Print out a pretty table of the word list.
printFrequencies :: WordCounter ()
printFrequencies = do
    st <- get
    let scale       = maxCount st
        wordWidth   = longestWord st
        screenWidth = 80
    liftIO . (printWordList screenWidth scale wordWidth) . toList . wordMap $ st

-- | Prints the frequencies of all words in all files as one table.
printFileWordFrequencies :: [FilePath] -> IO ()
printFileWordFrequencies files = evalStateT (countAllFiles >> printFrequencies) initialState
    where
        countAllFiles   = mapM_ countFile files
        countFile file  = getWordsFromFile file >>= countWords

-- | Get the list of files we are going to read in.  Currently just assumes all command-line arguments are files to process.
getFileNames :: IO [FilePath]
getFileNames = do
    args <- getArgs
    case args of
        files@(file:_) -> return files
        _              -> error "You must supply at least one file."


main = getFileNames >>= printFileWordFrequencies
