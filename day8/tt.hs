length2Nr :: Int -> Int
length2Nr 2 = 1
length2Nr 3 = 7
length2Nr 4 = 4
length2Nr 7 = 8
length2Nr x = -1

find8471 :: [Set Char] -> [(Int, Set Char)]
find8471 xs = map (first length2Nr) lengthLst
  where
    lengthLst = filter (\(x, y) -> x `elem` [2, 3, 4, 7]) . map (\x -> (length x, x)) $ xs

find906 :: [Set Char] -> [(Int, Set Char)] -> [(Int, Set Char)]
find906 xs _8471 = map find xs
  where
    find x
      | four `isSubsetOf` x = (9, x)
      | one `isSubsetOf`  x = (0, x)
      | otherwise           = (6, x)
    Just one = lookup 1 _8471
    Just four = lookup 4 _8471

find532 :: [Set Char] -> [(Int, Set Char)] -> [(Int, Set Char)]
find532 xs _8471906 = map find xs
  where
    find x
      | one `isSubsetOf`  x = (3, x)
      | x `isSubsetOf` nine = (5, x)
      | otherwise           = (2, x)
    Just one  = lookup 1 _8471906
    Just nine = lookup 9 _8471906

decode :: [Set Char] -> [(Set Char, Int)]
decode xs = map (\(x, y) -> (y, x)) (_8471906 ++ _532)
  where
    _8471 = find8471 xs
    _906 = find906 (filter (\x -> length x == 6) xs) _8471
    _8471906 = _8471 ++ _906
    _532 = find532 (filter (\x -> length x == 5) xs) _8471906

day8_1, day8_2 :: [String] -> Int
day8_1 input = length . filter (\x -> length x `elem` [2, 3, 4, 7]) . concatMap last $ daten
  where daten = map (map words . split' (== '|')) input

day8_2 input = sum . map toInt $ xs
  where
    Just xs = mapM (sequence . g . f . map (map fromList)) daten
    daten = map (map words . split' (== '|')) input
    g (xs, ys) = map (`lookup` xs) ys
    f xs = (decode . head $ xs , last xs)
    toInt xs = foldr (\x y -> x + 10 * y) 0 (reverse xs)