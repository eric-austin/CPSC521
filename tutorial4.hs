{-
Monads
Main idea: x -> m y, y -> m z
Want to be able to compose these to get x -> m z

eg. x -> [y], y -> [z] => x -> [z]

f :: x -> Maybe y, g :: y -> Maybe z => g . f :: x -> Maybe z
-}

data SF a = SS a | FF

{-
Functor law:
    fmap id x = x
    fmap (g . f) = (fmap g) . (fmap f)
-}

instance Functor SF where
    --fmap :: (a -> b) -> f a -> f b
    fmap f FF = FF
    fmap f (SS x) = SS (f x)

instance Applicative SF where
    --pure :: a -> f a
    pure x = SS x
    --(<*>) :: f (a -> b) -> f a -> f b
    (<*>) g x = case g of
                    FF -> FF
                    SS h -> fmap h x

instance Monad SF where
    --return :: a -> List a
    return x = SS x
    --(>>=) :: SF b -> (b -> SF c) -> a -> SF c
    (>>=) f g = case f of
                    FF -> FF
                    SS y -> g y
