import Data.List
import Data.Ord (comparing)
import System.IO

-- simple function summing numbers from 1 to 10
sumofnums = sum [1..10]

dadwv = [10, 11 .. 100]
lenofendless = length dadwv
dosomemath = [x | x <- dadwv, mod x 2 == 0, mod x 3 == 0]

factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

whatGrade :: Int -> String
whatGrade age
    | (age >= 4) && (age < 18) = "idiot"
    | otherwise = "old idiot!"

avgFucks :: Double -> Double -> String
avgFucks fucks days
    | avg < 1 = "loser"
    | avg < 1.6 = "well done fucker"
    | otherwise = "real motherfucker"
    where avg = fucks / days

-- task 1

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fibonacci' (n-1) + fibonacci' (n-2)

foo x
  | x < 0     = abs x
  | otherwise = y * 2
  where y = factorial x

fib n = loop 0 0 1
  where loop i curr next
          | i == n    = curr
          | otherwise = loop (i+1) next (curr+next)

countRoots :: Double -> Double -> Double -> Int
countRoots a b c
    | d < 0     = 0
    | d == 0    = 1
    | otherwise = 2
    where d = b*b - 4*a*c

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' 0 b = b
gcd' a b 
    | a > b  = gcd' (mod a b) b
    | a < b  = gcd' (mod b a) a 
    | otherwise = a

ackermann :: Int -> Int -> Int
ackermann m n 
    | m == 0 = n+1
    | m > 0 && n == 0 = ackermann (m - 1) 1
    | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))



-- distance :: (Double, Double) -> (Double, Double) -> Double
distance :: Floating a => (a, a) -> (a, a) -> a
distance a b = sqrt( (fst b - fst a)^2 + (snd b - snd a)^2)

modulus :: Floating a => (a, a) -> a
modulus = distance (0, 0)

complAdd :: Num a => (a, a) -> (a, a) -> (a, a)
complAdd (x1, y1) (x2, y2) = (x1+x2, y1+y2)


-- упражнение №9

replicate' :: Integral a => a -> a -> [a]
replicate' a b 
    | a == 0 = []
    | otherwise = b : replicate' (a - 1) b


concatVec :: Integral a => a -> a -> [a]
concatVec a b 
    | a == b = [a]
    | otherwise = a : concatVec (a + 1) b


take' :: Int -> [Int] -> [Int]
take' a (x:xs)
    | a == 0 = []
    | otherwise = x : take' (a - 1) xs


drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 lst = lst
drop' n (_:xs) = drop' (n-1) xs

isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime n = null [x | x<-[2..(n-1)], mod n x == 0]

primes :: Integral a => [a]
primes = filter isPrime [1..]

removeNth :: Integral a => a -> [a] -> [a]
removeNth a (x:xs)
    | null xs == True = [x]
    | mod x a == 0 = removeNth a xs
    | otherwise = x : removeNth a xs

map' f lst = [ f x | x<-lst]
filter' f lst = [x | x<-lst, f x]
devisors n = [ d | d<-[1..n], mod n d == 0]
descartes lst1 lst2 = [(x, y) | x<-lst1, y<-lst2]

flip' :: (Int->Int->Int)->(Int->Int->Int)
flip' f a b = f b a

takeWhile' f (x:xs)
    | f x == True = x : takeWhile' f xs
    | otherwise = []

minimum' :: [Int] -> Int
minimum' [] = error "WTF man?"
minimum' (x:xs) = foldr min x xs

reverse' :: [Int]->[Int]
reverse' [] = []
reverse' (x:xs) = foldr (:) [x] (reverse' xs)



compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress lst = (n, cnt) : compress rest
    where n = head lst
          cnt = length $ takeWhile (==n) lst
          rest = drop cnt lst

takeFsts :: [(Int, Int)]->[Int]
takeFsts [] = []
takeFsts (x:xs) = fst x : takeFsts xs

takeSnds :: [(Int, Int)]->[Int]
takeSnds [] = []
takeSnds (x:xs) = snd x : takeSnds xs

maxRepeated :: [Int]->Int
maxRepeated [] = 0
maxRepeated lst = maximum $ takeSnds $ compress lst

makeSet :: [Int]->[Int]
makeSet [] = []
makeSet (x:xs) = x : makeSet (filter (/=x) xs)

countAppears :: Int->[Int]->Int
countAppears a lst = length $ filter (== a) lst

histogram :: [Int]->[(Int, Int)]
histogram [] = []
histogram (x:xs) = (x, n) : histogram (filter (/= x) xs)
    where n = (countAppears x xs) + 1

maxDistance :: Floating a => [(a, a)]->a
maxDistance [] = 0
-- maxDistance lst = maximum [(distance x y) | x<-lst, y<-lst]

----------------------------------------------------------------------

sumProducts :: [[Int]]->Int
sumProducts [] = 0
sumProducts (x:xs) = sum ((foldr (*) 1 x) : [sumProducts xs])

occurrences :: [Int]->[Int]->[Int]
occurrences [] _ = []
--occurrences _ [] = []
occurrences (x:xs) l = countAppears x l : (occurrences xs (filter (/=x) l))

----------------------------------------------------------------------

mainDiag :: [[Int]]->[Int]
mainDiag [] = []
mainDiag m = [ (m!!id1)!!id1 | id1<-[0..n]]
    where n = (length m) - 1


isSquare :: [[Int]]->Bool
isSquare [] = False
isSquare m = and $ map (== (length m)) [length x | x<-m]

sndDiag :: [[Int]]->[Int]
sndDiag [] = []
sndDiag m = [ ((reverse m)!!id1)!!id1 | id1<-[n, (n-1)..0]]
    where n = (length m) - 1

--sndDiag = mainDiag . map reverse

----------------------------------------------------------------------

matchLenghts m = and $ map (== n) [length x | x<-m]
    where n = length $ head m

removeElem a [] = []
removeElem a lst = filter (/=a) lst

----------------------------------------------------------------------

setDiff :: [Int]->[Int]->[Int]
setDiff [] _ = []
setDiff x [] = x
setDiff x (y:ys) = setDiff (removeElem y x) ys


setUnion :: [Int]->[Int]->[Int]
setUnion s1 [] = s1
setUnion [] s2 = s2
setUnion x y = sort $ (setDiff x y) ++ y

setUnion2 s1 s2 = sort $ nub $ s1 ++ s2
setIntersect2 s1 s2 = filter (`elem` s2) s1
setDiff2 s1 s2 = filter (not . (`elem` s2)) s1

----------------------------------------------------------------------

getRev :: Floating a => a -> a -> [a] -> [a]
getRev a b lst = (map ((\x y z -> ((-x)/y) * z) a b) lst)
sumRows a b = zipWith (+) a b

findNonZero :: (Floating a, Eq a) => [[a]]->[a]
findNonZero (x:xs)
    | (head x) == 0.0 = findNonZero xs
    | otherwise = x

rowreduce :: (Eq a, Floating a) => [[a]]->[[a]]
rowreduce a = [sumRows x (getRev (head x) (head nonzero) nonzero) | x<-a, x /= nonzero]
    where nonzero = findNonZero a

----------------------------------------------------------------------

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ el | el<-xs, el<x ]
                ++ [x]
                ++ quicksort (filter (>=x) xs)

----------------------------------------------------------------------
                
type Point = (Double, Double)

distance1 :: Point -> Point -> Double
distance1 a b = sqrt( (fst b - fst a)^2 + (snd b - snd a)^2)

maxDistance1 :: [Point]->Double
maxDistance1 []  = 0
maxDistance1 lst = maximum [(distance1 x y) | x<-lst, y<-lst]

----------------------------------------------------------------------

type Item = (String, Integer)

takeSoon = filter 

soonestExpiring :: [Item] -> String
soonestExpiring its = fst $ minimumBy (comparing snd) [i | i<-its, snd i >= 0]

numberExpiring :: [Item] -> Int
numberExpiring its = length [i | i<-its, (snd i) < 0]

longestExpired :: [Item] -> String
longestExpired its = fst $ minimumBy (comparing snd) its

expiringItems :: [Item] -> (String, Int, String)
expiringItems its = (soonestExpiring its, numberExpiring its, longestExpired its)

----------------------------------------------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

height :: Tree Int -> Int
height Empty = 0
height (Node val left right) = 1 + max (height left) (height right)

----------------------------------------------------------------------

prune :: Tree Int -> Tree Int
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val left right) = Node val (prune left) (prune right)

----------------------------------------------------------------------

bloom :: Tree Int -> Tree Int
bloom Empty = Empty
bloom (Node val Empty Empty) = (Node val (Node val Empty Empty) (Node val Empty Empty))
bloom (Node val left right) = Node val (bloom left) (bloom right)

----------------------------------------------------------------------

findPath :: Int -> Tree Int -> [Int]
findPath _ Empty = []
findPath a (Node val Empty Empty)
    | a == val = [a]
    | otherwise = []
findPath a (Node val left right) 
    | a == val = [a]
--    | otherwise = (findPath a left) ++  ++ [val]
        
----------------------------------------------------------------------

treeMap :: (a -> a) -> Tree a -> Tree a
treeMap _ Empty = Empty
treeMap f (Node val Empty Empty) = (Node (f val) Empty Empty)
treeMap f (Node val left right) = (Node (f val) (treeMap f left) (treeMap f right))


data BST a = BEmpty | BNode a (BST a) (BST a)

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstinsert x BEmpty = BNode x BEmpty BEmpty
bstinsert x t@(BNode val left right)
    | x == val  = t
    | x < val   = BNode val (bstinsert x left) right
    | otherwise = BNode val left (bstinsert x right)

bstsize :: BST a -> Integer
bstsize BEmpty = 0
bstsize (BNode _ left right) = 1 + bstsize left + bstsize right

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstsearch _ BEmpty = False
bstsearch x (BNode val left right)
    | x == val  = True
    | x < val   = bstsearch x left
    | otherwise = bstsearch x right

bstFromList :: (Eq a, Ord a) => [a] -> BST a
bstFromList = foldr bstinsert BEmpty

-- забележете - тази функция няма ограничения за съдържания тип!
values :: BST a -> [a]
values BEmpty = []
values (BNode val left right) = values left ++ [val] ++ values right

bstSort :: (Eq a, Ord a) => [a] -> [a]
bstSort = values . bstFromList

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