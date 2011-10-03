module WordFreq.Printer.Histogram ( printWordList
                                   ) where
{- | This module is responsible for printing words lists.
 -}

import Text.Printf

import Control.Monad (when)

-- | Print out a line for a single word-count pair.
printWord :: Integer  -- | Width of the screen
          -> Integer  -- | Maximum count in the whole word list, for scaling the bar.
          -> Integer  -- | Maximum word length, so we can line up the bars on the right.
          -> String   -- | The word itself.
          -> Integer  -- | The number of times the word occurred.
          -> IO ()
printWord screenWidth maxVal maxWordLen word v =
    let format      = ("%-" ++ show maxWordLen ++ "s: %s\n")
        maxBarWidth = screenWidth - maxWordLen - 2
        barWidth    = round $ fromIntegral v / fromIntegral maxVal * fromIntegral maxBarWidth
        bar         = replicate barWidth '#'
     in printf format word bar

-- | Prints the entire word list as a histogram.
printWordList :: Integer -- | Width of the screen
              -> Integer -- | Maximum value in the list, used for scaling the bar.
              -> Integer -- | Maximum word length, for lining up the output.
              -> [(String, Integer)]  -- | The words and their values, as returned by functions such as Data.Map.toList.
              -> IO ()
printWordList screenWidth maxVal maxWordLen words =
    mapM_ guardedPrint words
    where
        guardedPrint (w, v) = when (v > 0) $
                              printWord screenWidth maxVal maxWordLen w v

