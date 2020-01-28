main :: IO ()
main = return ()

greaterInList :: Integer -> [Integer] -> [Integer]
greaterInList y xs = [x | x<-xs, x > y]

greaterInList2 :: Integer -> [Integer] -> [Integer]
greaterInList2 y (x:xs) = if x > y
                          then x:(greaterInList2 y xs)
                          else (greaterInList2 y xs)
