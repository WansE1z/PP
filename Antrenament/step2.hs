addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Radu"
charName 'b' = "Ana"
charName x = "No name for this one"

addVector :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVector a b = (fst a + fst b, snd a + snd b)

first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

xs = [(1,3) , (4,3) , (2,4) ,(5,3) ,(5,6) , (3,1)]

head' :: [a] -> a
head' [] = error "Nope."
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi
    | bmi <= 18.5 = "."
    | bmi <= 25.0 = ","
    | bmi <= 30.0 = "lala"
    | otherwise = "fat"

max' :: (Ord a) => a -> a -> a
max' a b 
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTell1 :: (RealFloat a) => a -> a -> String  
bmiTell1 weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname   

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmi :: (RealFloat a) => [(a,a)] -> [a]
calcBmi xs = [bmi | (w,h) <- xs, let bmi = w/ h ^2]

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  