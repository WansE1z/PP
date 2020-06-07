data Queue a = Q [a] [a] deriving Show

isEmpty :: (Queue a) -> Bool 
isEmpty (Q [] []) = True 
isEmpty _ = False 

del :: (Queue a) -> (Queue a) 
del (Q xs []) = Q [] $ tail $ reverse xs 
del (Q xs (y:ys)) = Q xs ys

top :: (Queue a) -> a
top (Q xs []) = last xs
top (Q _ (x:_)) = x

ins :: a -> (Queue a) -> (Queue a)
ins x (Q xs ys) = Q (x:xs) ys
