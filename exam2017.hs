import Data.List
import Data.Ord (comparing)
import System.IO

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

takeConcat :: Tree [Int] -> [Int]
takeConcat Empty = []
takeConcat (Node val Empty Empty) = val
takeConcat (Node val left right) = val ++ takeConcat left ++ takeConcat right

tranferTree :: Tree Int -> Tree [Int]
tranferTree Empty = Empty
tranferTree (Node val Empty Empty) = (Node [val, val] Empty Empty)
tranferTree (Node val left right) = (Node [val, val] (tranferTree left) (tranferTree right))

makeInterval :: Tree [Int] -> Tree [Int]
makeInterval Empty = Empty
makeInterval (Node val Empty Empty) = (Node val Empty Empty)
makeInterval (Node val left right) = (Node [minimum subTree, maximum subTree] (makeInterval left) (makeInterval right))
    where subTree = takeConcat (Node val left right)

intervalTree :: Tree Int -> Tree [Int]
intervalTree tree = makeInterval $ tranferTree tree

----------------------------------------------------------------------

sumOfSqrs = [x^2 + (x+1)^2 | x<-[1..]]

----------------------------------------------------------------------

averageVideo :: [(String, Int)] -> String
averageVideo vids = fst $ maximumBy (comparing snd) [i | i<-vids, snd i < avg]
    where avg = quot (sum $ [x | x<-(map snd vids)]) (length vids)

----------------------------------------------------------------------

largestInterval :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> [Int]
largestInterval f g a b = [x | x<-[a..b], f x == g x]