{-# LANGUAGE OverloadedStrings #-}
module WordFreq.Reader.Text ( readWords
                            , Word
                            , toString
                            , wordLength) where
{- | Get the appropriate input data as a list of words.  It can either be from
 - STDIN, if no command line arguments are given, or from files, where each is
 - listed on the command line.
 -}

import qualified Data.Text as T (toLower, words, Text(..), filter, null, unpack, length)
import Data.Text.Encoding as E (decodeUtf8)
import System.Environment (getArgs)
import Data.ByteString as BS (getContents, ByteString, readFile)

-- | A simple type alias for the type we will be using for Words.
type Word = T.Text

-- | The function required to convert the internal word representation to a String.
toString = T.unpack

-- | The function used to calculate word lengths.
wordLength = T.length

-- | Read raw data, reading all command-line arguments as files or reading from STDIN if no arguments are given.
readWords :: IO [Word]
readWords = do
    args <- getArgs
    case args of
        []    -> readStdin
        files -> readFiles files

----------------------------------------
-- * Internal functions
----------------------------------------

-- | Normalizes a word by removing capitalization and punctuation.
normalizeWord :: Word -> Word
normalizeWord = T.filter (`notElem` "*,.-!?<>/\";:][}{)(@#$%^&`~-_=+") . T.toLower

-- | Clean up the word list, normalizing and removing empty words.
cleanWords :: ByteString -> [Word]
cleanWords = filter (not . T.null) . map normalizeWord . T.words . E.decodeUtf8

-- Get words from STDIN.
readStdin = cleanWords `fmap` BS.getContents

-- Get words from files
readFiles :: [FilePath] -> IO [Word]
readFiles files = getWord files []

-- | Allows for lazy loading of the word lists as it doesn't require scanning
-- the word list to find the end, like (++) does.  Consumes all the words in a
-- list, then reads from a file to repopulate the list, aborting if no more
-- files are left to read.
getWord :: [FilePath] -> [Word] -> IO [Word]
getWord []     []     = return []
getWord (f:fs) []     = getWordsFromFile f >>= getWord fs
getWord fs     (w:ws) = fmap (w :) $ getWord fs ws

-- | Returns all the words in a file.
getWordsFromFile :: FilePath -> IO [Word]
getWordsFromFile file = cleanWords `fmap` BS.readFile file
