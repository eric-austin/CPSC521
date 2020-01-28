--Eric Austin
--30037742
--CPSC 521 Assignment 1


--------------------------------------------------------------------------------
--Question 1
--code for app given in lecture notes
app :: ([a], [a]) -> [a]
app (xs, ys) = foldr (:) ys xs

--code for rev given in lecture notes
rev :: [a] -> [a]
rev l = shunt [] l
    where shunt ys [] = ys
          shunt ys (x:xs) = shunt (x:ys) xs

--code for List data type given in tutorial
data List a = Empty | Cons a (List a) deriving Show

foldList :: (a -> c -> c) -> c -> List a -> c
foldList f c0 Empty = c0
foldList f c0 (Cons x xs) = f x (foldList f c0 xs)

append :: (List a, List a) -> List a
append (xs, ys) = foldList Cons ys xs


--------------------------------------------------------------------------------
--Question 2
flatten :: [[a]] -> [a]
flatten xs = foldr (\as bs -> app(as, bs)) [] xs

flatten' :: List (List a) -> List a
flatten' xs = foldList (\as bs -> append(as, bs)) Empty xs


--------------------------------------------------------------------------------
--Question 3
greaterinlist :: Integer -> [Integer] -> [Integer]
greaterinlist n [] = []
greaterinlist n (x:xs) | x > n = x:(greaterinlist n xs)
                       | otherwise = greaterinlist n xs

greaterinlist' :: Integer -> [Integer] -> [Integer]
greaterinlist' n xs = foldr (\x xs -> if x > n then x:xs else xs) [] xs


--------------------------------------------------------------------------------
--Question 4
lexInt :: [Int] -> [Int] -> Bool
lexInt [] ys = False
lexInt xs [] = True
lexInt (x:xs) (y:ys) | x > y = True
                     | x < y = False
                     | otherwise = lexInt xs ys

lexOrd :: (Ord a) => [a] -> [a] -> Bool
lexOrd [] ys = False
lexOrd xs [] = True
lexOrd (x:xs) (y:ys) | x > y = True
                     | x < y = False
                     | otherwise = lexOrd xs ys

--------------------------------------------------------------------------------
--Question 5
msplit :: [a] -> ([a], [a])
msplit ls = foldr (\x (ys, zs) -> (zs, x:ys)) ([], []) ls

mergeInt :: ([Integer], [Integer]) -> [Integer]
mergeInt ([], []) = []
mergeInt ([], y:ys) = y:(mergeInt ([], ys))
mergeInt (x:xs, []) = x:(mergeInt (xs, []))
mergeInt (x:xs, y:ys) | x <= y = x:(mergeInt (xs, y:ys))
                      | otherwise = y:(mergeInt (x:xs, ys))

mergesortInt :: [Integer] -> [Integer]
mergesortInt [] = []
mergesortInt [x] = [x]
mergesortInt xs = mergeInt(mergesortInt(fst(msplit xs)), mergesortInt(snd(msplit xs)))

merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], []) = []
merge ([], y:ys) = y:(merge ([], ys))
merge (x:xs, []) = x:(merge (xs, []))
merge (x:xs, y:ys) | x <= y = x:(merge (xs, y:ys))
                   | otherwise = y:(merge (x:xs, ys))

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge(mergesort(fst(msplit xs)), mergesort(snd(msplit xs)))


--------------------------------------------------------------------------------
--Question 6
qsplit :: (Ord a) => [a] -> ([a], [a])
qsplit [] = ([], [])
qsplit [x] = ([x], [])
qsplit (x:xs) = (smaller, bigger)
    where smaller = [y | y <- xs, y <= x]
          bigger = [y | y <- xs, y > x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort(fst (qsplit (x:xs))) ++ [x] ++ quicksort(snd (qsplit (x:xs)))


--------------------------------------------------------------------------------
--Question 7
member :: (Eq a) => a -> [a] -> Bool
member x ys = foldr (\y b -> if (y == x) then (True || b) else (False || b)) False ys

member' :: (Eq a) => a -> [a] -> Bool
member' x [] = False
member' x (y:ys) | x == y = True
                 | otherwise = member' x ys


--------------------------------------------------------------------------------
--Question 8
relgrp :: (a -> b -> Bool) -> [a] -> [b] -> [(a,[b])]
relgrp rel [] ys = []
relgrp rel (x:xs) ys = (x, [y | y <- ys, rel x y]):(relgrp rel xs ys)


--------------------------------------------------------------------------------
--Question 9
group :: (a -> a -> Bool) -> [a] -> [[a]]
group [] = []
