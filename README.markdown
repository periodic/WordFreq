# WordFreq

Author: Drew Haven

A simple word-frequency counter for the CS240H Lab1.

## Building

A simple "cabal configure && cabal build" should do it.  After that you can find the executable in ./dist/build/wordfreq/wordfreq.

## Running

To run the program, simply invoke it with a list of files as arguments.  As long as at least one argument is supplied it will read them all in and count all words, printing a histogram to the console.  If no arguments are given it will read from standard input.  The output assumes an 80-column width terminal and it may be quite long if the text has many words.

## Architecture

The program is broken up into three parts, there is a reader component, a counter component and a printer component.

The reader is responsible for getting the data from the appropriate data source, be it files or standard input.  It breaks it up into words and cleans up the words.

The counter counts up the word frequencies and tracks any statistics we need.

The printer prints out the data in a human-readable format.
