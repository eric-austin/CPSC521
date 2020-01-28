data List a = Cons a (List a) | Nil deriving Eq

--define the max-elem-norm ordering on lists (L_\infty norm)
--The list x > y iff the maximum element of x is greater than
--the maximum element of y

--use a partial function to help (not defined on empty List)
maxList :: (Ord a) => (List a) -> a
maxList Nil = undefined
maxList (Cons x Nil) = x
maxList (Cons x xs) = max x (maxList xs)

--need compare :: a -> a -> Ordering
--where data Ordering = EQ | LT | GT
instance (Ord a) => Ord (List a) where
    compare Nil Nil = EQ
    compare Nil (Cons x xs) = LT
    compare (Cons x xs) Nil = GT
    compare (Cons x xs) (Cons y ys) = compare (maxList (Cons x xs)) (maxList (Cons y ys))
