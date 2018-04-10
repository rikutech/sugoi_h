-- 配列の先頭を取り出す(パターンマッチで空のとき例外を投げるようにしている)
head' :: [a] -> a
head' [] = error "err"
head' (x:_) = x

-- ガードとwhere(whereで束縛した変数)
bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "痩せすぎです"
  | bmi <= normal = "標準体型です"
  | bmi <= fat = "肥満気味です"
  | otherwise = "人間じゃない可能性があります"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

-- takeの実装

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let lessOrEqual = [a | a <- xs, a <= x]
      grater = [a | a <- xs, a > x]
  in quicksort lessOrEqual ++ [x] ++ quicksort grater
