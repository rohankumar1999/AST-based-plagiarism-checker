import Control.Monad (void)
import Ast
import Compare




main :: IO ()
main = do
    list1 <- showParsedExp "abs.imp"
    list2 <- showParsedExp "abs1.imp"
    print list1
    print list2
    let overlaps = findOverlappingSubarrays list1 list2
    print $ overlaps