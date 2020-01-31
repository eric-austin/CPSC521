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

lexOrd' :: (Ord a) => List a -> List a -> Bool
lexOrd' Empty _ = False
lexOrd' _ Empty = True
lexOrd' (Cons x xs) (Cons y ys) | x > y = True
                               | x < y = False
                               |otherwise = lexOrd' xs ys


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
nbr :: (Integral a) => a -> a -> Bool
nbr x y | abs (x - y) <= 2 = True
        | otherwise = False

subgroup :: (a -> a -> Bool) -> [a] -> [a]
subgroup _ [] = []
subgroup _ [x] = [x]
subgroup pr (x:xs) | pr x (head xs) = [x] ++ (subgroup pr xs)
                   | otherwise = [x]

group :: (a -> a -> Bool) -> [a] -> [[a]]
group _ [] = []
group pr xs = ys:(group pr (drop (length ys) xs))
    where ys = subgroup pr xs


--------------------------------------------------------------------------------
--Question 10
subset :: [a] -> [[a]]
subset xs = foldr (\x acc -> (map (x:) acc) ++ acc) [[]] xs


--------------------------------------------------------------------------------
--Question 11
perm :: [a] -> [[a]]
perm xs = foldr (\x acc -> (removeLayer(map (insert x) acc))) [[]] xs

removeLayer :: [[[a]]] -> [[a]]
removeLayer xs = foldr (\x acc -> x ++ acc) [] xs

insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x (y:ys) = [(x:y:ys)] ++ (map (y:) (insert x ys))

--------------------------------------------------------------------------------
--Question 12
cycDecomp :: (Eq a) => [a] -> [[[a]]]
cycDecomp xs = map (\ts -> buildCycles [] ts ts) (map (zip xs) (perm xs))

alreadyInCycle :: (Eq a) => a -> [a] -> Bool
alreadyInCycle _ [] = False
alreadyInCycle x (y:ys) | x == y = True
                        | otherwise = alreadyInCycle x ys

alreadyInAnyCycle :: (Eq a) => a -> [[a]] -> Bool
alreadyInAnyCycle _ [] = False
alreadyInAnyCycle x (y:ys) = (alreadyInCycle x y) || (alreadyInAnyCycle x ys)

nextTuple :: (Eq a) => (a, a) -> [(a, a)] -> (a, a)
nextTuple (x, y) [] = (x, y)
nextTuple (x, y) ((u, v):ls) | y == u = (u, v)
                             | otherwise = nextTuple (x, y) ls

buildCycles :: (Eq a) => [[a]] -> [(a, a)] -> [(a, a)] -> [[a]]
buildCycles cycs _ [] = cycs
buildCycles cycs [] _ = cycs
buildCycles cycs ((x, y):zs) ls | alreadyInAnyCycle x cycs = buildCycles cycs zs ls
                                | otherwise = buildCycles ((buildCycle [] (x, y) ls):cycs) zs ls

buildCycle :: (Eq a) => [a] -> (a, a) -> [(a, a)] -> [a]
buildCycle cyc (x, y) zs | alreadyInCycle x cyc = cyc
                         | otherwise = buildCycle (cyc ++ [x]) (nextTuple (x, y) zs) zs


--------------------------------------------------------------------------------
--Question 15
polysplit :: [(Float, [Int])] -> ([(Float, [Int])], [(Float, [Int])])
polysplit [] = ([], [])
polysplit [x] = ([x], [])
polysplit (x:xs) = (smaller, bigger)
    where smaller = [y | y <- xs, lexOrd (snd x) (snd y)]
          bigger = [y | y <- xs, not(lexOrd (snd x) (snd y))]

polyquicksort :: [(Float, [Int])] -> [(Float, [Int])]
polyquicksort [] = []
polyquicksort [x] = [x]
polyquicksort (x:xs) = polyquicksort(fst (polysplit (x:xs))) ++ [x] ++ polyquicksort(snd (polysplit (x:xs)))

addTerms :: (Float,[Int]) -> (Float,[Int]) -> (Float,[Int])
addTerms (x, xs) (y, ys) = (x + y, xs)

multTerms :: (Float,[Int]) -> (Float,[Int]) -> (Float,[Int])
multTerms (x, xs) (y, ys) = (x * y, (addpowers xs ys))
    where addpowers ms [] = ms
          addpowers [] ns = ns
          addpowers (m:ms) (n:ns) = (m + n):(addpowers ms ns)

combineTerms :: [(Float,[Int])] -> [(Float,[Int])]
combineTerms [] = []
combineTerms [t] = [t]
combineTerms (t:ts) | (snd t) == (snd (head ts)) = combineTerms ((addTerms t (head ts)):(tail ts))
                    | otherwise =  (t:(combineTerms ts))

normpoly :: [(Float,[Int])] -> [(Float,[Int])]
normpoly [] =[]
normpoly [x] = [x]
normpoly xs = combineTerms (polyquicksort xs)

addnormpoly :: [(Float,[Int])]  -> [(Float,[Int])] -> [(Float,[Int])]
addnormpoly [] ys = ys
addnormpoly xs [] = xs
addnormpoly (x:xs) (y:ys) | (snd x) < (snd y) = x:(addnormpoly xs (y:ys))
                          | (snd x) > (snd y) = y:(addnormpoly (x:xs) ys)
                          | otherwise = (addTerms x y):(addnormpoly xs ys)

addpoly :: [(Float,[Int])]  -> [(Float,[Int])] -> [(Float,[Int])]
addpoly xs ys = addnormpoly (normpoly xs) (normpoly ys)

multpoly :: [(Float,[Int])]  -> [(Float,[Int])] -> [(Float,[Int])]
multpoly [] ys = ys
multpoly xs [] = xs
multpoly xs ys = normpoly (multpoly' xs ys)
    where multpoly' xs [] = xs
          multpoly' [] ys = []
          multpoly' (x:xs) ys = (map (multTerms x) ys) ++ (multpoly' xs ys)


--------------------------------------------------------------------------------
--Question 19
data Rose a = RS [(a,Rose a)]

width:: Rose Int -> Int
width (RS []) = 0
width (RS xs) = foldr (\(x, rose) acc -> if (x + (width rose)) > acc then (x + width rose) else acc) 0 xs
