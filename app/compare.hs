module Compare (
  filterSubarrays,
  findOverlappingSubarrays
) where

import Data.List (tails, inits)

-- findOverlappingSubarrays :: Eq a => [a] -> [a] -> [[a]]
findOverlappingSubarrays xs ys =
    length $ filterSubarrays [sub | sub <- subarrays xs, sub `elem` subarrays ys]

subarrays :: [a] -> [[a]]
subarrays = concatMap tails . inits

-- Check if the first list is a subarray of the second list
isSubarrayOf :: Eq a => [a] -> [a] -> Bool
isSubarrayOf [] _ = True
isSubarrayOf _ [] = False
isSubarrayOf (x:xs) (y:ys)
    | x == y = isSubarrayOf xs ys || isSubarrayOf (x:xs) ys
    | otherwise = isSubarrayOf (x:xs) ys

-- Function to filter out lists that are subarrays of any other list
filterSubarrays :: Eq a => [[a]] -> [[a]]
filterSubarrays l = filter (\x -> not (any (\y -> x /= y && isSubarrayOf x y) l) && length x >= 10) l