# WordFreq

Author: Drew Haven

A simple word-frequency counter for the CS240H Lab1.

## Building

A simple "cabal configure && cabal build" should do it.  After that you can find the executable in ./dist/build/wordfreq/wordfreq.

## Running

To run the program, simply invoke it with a list of files as arguments.  As long as at least one argument is supplied it will read them all in and count all words, printing a histogram to the console.  If no arguments are given it will read from standard input.  The output assumes an 80-column width terminal and it may be quite long if the text has many words.

## Architecture

The program is broken up into three parts, there is a reader component, a counter component and a printer component.

The reader is responsible for getting the data from the appropriate data source, be it files or standard input.  It breaks it up into words and cleans up the words.  The type used to store the words, e.g. String, ByteString or Text, is hidden in this module, though it exports functions to determine length and convert to a string, and the internal type must implement Ord and Eq.  This is essentially a lexer.

The counter counts up the word frequencies and tracks any statistics we need.  It does this by iterating over the list of words and building up internal state where it counts frequencies, tracks the longest word, and tracks the maximum frequency found so far, as those are required for printing.

The printer prints out the data in a human-readable format.  Currenty modules include a histogram, which prints a simple ascii histogram of all the words.
