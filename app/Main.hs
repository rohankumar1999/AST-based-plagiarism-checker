module Main where

import Control.Monad (void)
import Ast
import Compare




main :: IO ()
main = do
    list1 <- showAst "abs.imp"
    -- print list1
    list2 <- showAst "abs1.imp"
    -- print list2
    let overlaps = findOverlappingSubarrays list1 list2
    putStr "Number of overlaps: "
    putStrLn $ show overlaps
