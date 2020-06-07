data Tree a = Node {
    key :: a,
    children :: [Tree a]
} deriving (Eq,Show)

height :: Tree a -> Int
height (Node _ []) = 0
height (Node _ lista) = 1 + maximum (map height lista) 

frunze :: Tree a -> Int
frunze (Node _ []) = 1
frunze (Node _ lista) = sum (map frunze lista)

filterTree :: (a -> Bool) -> Tree a -> Tree a
filterTree f (Node k child) = Node k $ map (filterTree f) $ filter (f . key) child 