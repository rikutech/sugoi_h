-- 第一引数の関数を第二引数の値に2回適用する
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)

-- zipWithの実装
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
