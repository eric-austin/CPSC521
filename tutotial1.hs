app::([a], [a]) -> [a]

curry::((a, b) -> c) -> a -> b -> c
curry f x y = f(x, y)

(curry app)::[a] -> [a] -> [a]
