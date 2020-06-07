data HashSet = HS [(Int, [Int])] deriving (Eq,Show,Ord)

values' :: HashSet -> Int -> [Int]
values' (HS hs) h 
    | (length hs) == 0 = []
    | (fst $ head hs) == h = (snd $ head hs)
    | otherwise = values' (HS (tail hs)) h

existahash :: HashSet -> Int -> Bool
existahash (HS hs) h
    | (length hs) == 0 = False
    | h == (fst $ head hs) = True
    | otherwise = existahash (HS (tail hs)) h

insert2' :: HashSet -> Int -> [(Int, [Int])]
insert2' (HS hs) h 
    | (length hs) == 0 = []
    | (h `mod` 2) == (fst (head hs)) = [(h `mod` 2, [h] ++ (snd (head hs)))] ++ insert2' (HS (tail hs)) h
    | otherwise = [(head hs)] ++ insert2' (HS (tail hs)) h

insert' :: HashSet -> Int -> HashSet  
insert' (HS hs) h 
    | (existahash (HS hs) h) == False = (HS (hs ++ [(h `mod` 2, [h])]))
    | otherwise = HS (insert2' (HS hs) h)

map' :: (Int -> Int) -> HashSet -> HashSet
map' f (HS hs) = HS $ map (\(x, xs) -> (x, map f xs)) hs
