import Data.List
import Data.Ord (comparing)
import System.IO

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)


paths :: Tree a -> [[a]] 
paths (Node a Empty Empty) = [[a]]
paths (Node x left right) = map (x:) (paths left ++ paths right)

path :: Tree Int -> Int -> Either Bool [Int]
path (Node val Empty Empty) elem
    | elem == val = Right [val]
    | otherwise = Left False
path (Node val left right) elem 
    | elem == val = Right [val]
--    | otherwise = map (val:) ((path left elem) || (path right elem))

-----------------------------------------------------------------------

findSameElem :: [Int]->[[Int]]->Bool
findSameElem lst (x:xs)
    | (head lst) `elem` x && findSameElem lst xs == False = findSameElem (tail lst) (x:xs)
    | otherwise = True

--allEqual :: [[Int]] -> [(Int->Int)] -> [Int]
--allEqual sets funcs 

-----------------------------------------------------------------------

l = [("A",[("p",6),("q",9)]),("B",[("p",2),("q",3)]),("C",[("p",3), ("q",7)]), ("D",[("p",4),("q",6)])]

type Ingredient = (String, Integer)
type Medicine = (String, [Ingredient])

takeFsts :: [(String, Integer)]->[String]
takeFsts [] = []
takeFsts (x:xs) = fst x : takeFsts xs

takeSnds :: [(String, Integer)]->[Integer]
takeSnds [] = []
takeSnds (x:xs) = snd x : takeSnds xs

isSubstitute :: Medicine -> Medicine -> Bool
isSubstitute (_, (a:as)) (_, (b:bs)) = (foldr (quot) (snd a) (takeSnds as) == foldr (quot) (snd b) (takeSnds bs))
     && (takeFsts (a:as) == takeFsts (b:bs))

bestSubstitute :: Medicine -> [Medicine] -> Medicine
bestSubstitute med meds = maximumBy (comparing snd) [i | i<-meds, snd i <= snd med]


freeTree :: Tree Int
freeTree =   
    Node 1
        (Node 2 
            (Node 3  
                (Node 4 Empty Empty)  
                (Node 5 Empty Empty)  
            )  
            (Node 6  
                (Node 7 Empty Empty)  
                (Node 8 Empty Empty)  
            )  
        )  
        (Node 9 
            (Node 10 Empty Empty)
            (Node 13  
                (Node 14 Empty Empty)                 
                (Node 15 Empty Empty)  
            )
        )