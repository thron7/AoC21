ones, zeros :: [Char] -> Int
ones  = length . filter (== '1')
zeros = length . filter (== '0')

bin2Int :: [Bool] -> Int
bin2Int = foldr (\x y -> fromEnum x + 2 * y) 0 . reverse

day3_1, day3_2 :: [[Char]] -> Int
day3_1 input = gamma * epsilon
  where gamma = bin2Int lst 
        epsilon = bin2Int . map not $ lst
        lst = map (\col -> ones col > zeros col) t
        t = transpose input
        
day3_2 input = oxygen * co2
  where oxygen = toInt (>=) input
        co2 = toInt (<) input
        toInt comp = bin2Int . map (== '1') . calc comp []
        
        calc comp bits [xs] = reverse bits ++ xs
        calc comp bits xss = calc comp (bit : bits) xss'
          where heads = map head xss
                bit = if ones heads `comp` zeros heads then '1' else '0'
                xss' = map tail . filter ((== bit) . head) $ xss 