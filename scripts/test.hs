-- 配列の先頭を取り出す(パターンマッチで空のとき例外を投げるようにしている)
head' :: [a] -> a
head' [] = error "err"
head' (x:_) = x

-- ガードとwhere
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
