data MultiMap k v = MM [(k, [v])] deriving (Eq,Show,Ord)

lookup' :: Eq k => k -> MultiMap k v -> [v]
lookup' k (MM multmap) = 
    foldr (\x acc -> if (fst x) == k then acc ++ (snd x) else acc) [] multmap

existacheie :: Eq k => MultiMap k v -> k -> Bool
existacheie (MM multmap) k
    | (length multmap) == 0 = False
    | k == ( fst $ head multmap) = True
    | otherwise = existacheie (MM (tail multmap)) k

insert' :: Eq k => MultiMap k v -> k -> v -> MultiMap k v
insert' (MM multmap) k v 
    | (existacheie (MM multmap) k) == True = (MM multmap)
    | otherwise = MM (multmap ++ [(k, [v])]) 
     
map' :: Num k => (v -> v1) -> MultiMap k v -> MultiMap k v1
map' f (MM multmap) = MM $ map (\(x,y) -> (x,map f y)) multmap
