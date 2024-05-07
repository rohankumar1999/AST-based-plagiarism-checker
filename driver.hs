import Control.Monad (void)
import Ast
import Compare




main :: IO ()
main = do
    list1 <- showAst "abs.imp"
    list2 <- showAst "abs1.imp"
    -- print list1
    -- print list2
    let overlaps = findOverlappingSubarrays list1 list2
    print "Number of overlaps: "
    print $ overlaps