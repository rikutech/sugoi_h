-- 第一引数の関数を第二引数の値に2回適用する
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

-- zipWithの実装
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 5-3

-- mapの実装

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filterの実装
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs
