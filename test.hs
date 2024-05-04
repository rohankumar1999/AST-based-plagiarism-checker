import Control.Monad (void)
import Ast
import Data.List (tails, inits)

findOverlappingSubarrays :: Eq a => [a] -> [a] -> [[a]]
findOverlappingSubarrays xs ys =
    [sub | sub <- subarrays xs, sub `elem` subarrays ys, length sub > 10]

subarrays :: [a] -> [[a]]
subarrays = concatMap tails . inits

main :: IO ()
main = do
    list1 <- showParsedExp "abs.imp"
    list2 <- showParsedExp "abs1.imp"
    print list1
    print list2
    let overlappingSubarrays = findOverlappingSubarrays list1 list2
    print overlappingSubarrays