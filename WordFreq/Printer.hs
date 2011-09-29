module WordFreq.Printer where

import Text.Printf

-- | Print out a line for a single word-count pair.
printWord :: (Show word) => Integer -> Integer -> Integer -> word -> Integer -> IO ()
printWord screenWidth maxVal maxWordLen word v =
    let format   = ("%" ++ show maxWordLen ++ "s: %s\n")
     in printf format (show word) (bar v maxVal barWidth)
    where
        barWidth = screenWidth - maxWordLen - 2
        bar v scale width = replicate (truncate $ fromIntegral v / fromIntegral scale * fromIntegral width) '#'

printWordList :: (Show word) => Integer -> Integer -> Integer -> [(word, Integer)] -> IO ()
printWordList screenWidth maxVal maxWordLen words = 
    mapM_ (uncurry $ printWord screenWidth maxVal maxWordLen) words
