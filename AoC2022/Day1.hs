{-# LANGUAGE RecordWildCards, BangPatterns, ViewPatterns, LambdaCase, TupleSections, PatternGuards, FlexibleContexts, NoMonomorphismRestriction, PartialTypeSignatures #-}

module Main where

solve1 = maximum

solve2 = sum . take 3 . sortOn negate

main = readFile "Day1.txt" >>= 
    (parseStr ((parseLines . parseLines) integer)
        >>>  fmap (solve2 . map sum) >>> print)
