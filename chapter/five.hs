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

-- 5-5 畳み込み


-- 左畳み込みによるreverseの実装
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- 右畳み込みによるmapの実装
-- :は++より圧倒的に早いのでlistを返すときは右畳み込みを使う
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1はlistの先頭(foldr1なら末尾)を初期アキュムレーターとして使う
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)

sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- 関数合成
twiceReverse :: (Num a) => [a] -> [a]
twiceReverse = reverse . map (\x -> x * 2)
