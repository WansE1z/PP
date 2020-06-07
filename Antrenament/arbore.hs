data Tree a = Node a [Tree a]

height :: Tree a -> Int
height (Node _ []) = 0
height (Node _ lista) = 1 + maximum (map height lista) 

frunze :: Tree a -> Int
frunze (Node _ []) = 1
frunze (Node _ lista) = sum (map leaves lista)