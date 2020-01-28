--foldr
--intuition: overwriting type constructors with functions

nonsense :: [a] -> Int
nonsense [] = 0
nonsense (x:xs) = (\a b -> b + 1) x (nonsense xs)

data List a = Empty
            | Cons a (List a)
            deriving (Eq, Show)

foldList :: (a -> b -> b) -> b -> List a -> b
foldList f z Empty = z
foldList f z (Cons x xs) = f x (foldList f z xs)

translate :: List a -> [a]
translate xs = foldList (:) [] xs

length' :: List a -> Integer
length' xs = foldList (\a b -> b + 1) 0 xs

--(Cons 1 Empty, Cons 2 Empty) |--> Cons 1 (Cons 2 Empty)

append :: (List a, List a) -> List a
append (xs, ys) = foldList Cons ys xs

--instead of [1,2,3,4], we have Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
--we should have a fold function that replaces "Cons" with a function, and Empty with a constant
--fold :: (a -> b -> b) -> b -> List a -> b
--what it should do
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))) to f 1 (f 2 (f 3 (f 4 z)))
