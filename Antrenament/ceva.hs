import Data.List

f = \x y -> x + y -- f 1 2 = 3

{- 
2 + 3 == (+) 2 3

(2 +) == \x -> 2 + x
(+ 2) == \x -> x + 2
(- 2) == -2
(2 -) == \x -> 2 - x

pentru a afla tipul unei functii -> :t (+) -> (+) :: Num a => a -> a -> a

1:3:5:[] == [1,3,5]

[1,2,3] -> 
    head = 1
    last = 3
    tail = [2,3]
    init = [1,2]

[1,2,3] ++ [4,5] = [1,2,3,4,5]

[x | x <- [0,2 ..], x 'mod' 3 == 0] Lista numerelor naturale pare, divizibile cu 3

(1,2) - pereche
fst (1,2) -> 1
snd (1,2) -> 2

-}

factorial_if x = 
    if x < 1 then 1 else x * factorial_if (x-1)

factorial_guards x 
    | x < 1 = 1
    | otherwise = x * factorial_guards (x-1)

factorial_case x = case x < 1 of
    True -> 1
    _ -> x * factorial_case (x-1)

factorial_pm 0 = 1
factorial_pm x = x * factorial_pm (x - 1)

length' [] = 0
length' (_ : xs) = 1 + length' xs

{-

    map :: (a -> b) -> [a] -> [b]
    filter :: (a -> Bool) -> [a] -> [a]
    foldl :: (a -> b -> a) -> a -> [b] -> a
    zip :: [a] -> [b] -> [(a, b)]
    zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

map (+ 2) [1, 2, 3] ->  [3, 4, 5]

filter odd [1, 2, 3, 4] -> [1, 3]

foldl (+) 0 [1, 2, 3, 4] -> 10

foldl (-) 0 [1, 2] -> -3 

foldr (-) 0 [1, 2] -> -1 

zip [1, 2] [3, 4] -> [(1, 3), (2, 4)]

zipWith (+) [1, 2] [3, 4] ->  [4, 6]

let id1 = expr1 in expr

-}
g = let x = y + 1
        y = 2 -- x = 3, y = 2
        (z,t) = (2,5) -- z = 2, t = 5
        f n = n * y -- n * 2
    in (x + y, f 3, z + t) -- (5,6,7)

{-

naturals = iter 0
    where iter x = x : iter (x + 1) -- lista numerelor naturale

head naturals -- 0
take 10 naturals -- [0,1,2,3,4,5,6,7,8,9,10]

-}

naturals' = iterate (+1) 0 -- iterate vrea 2 parametrii (conditie, start) -> (+1, 0)
powsOfTwo = iterate (*2) 1 -- [1,2,4,8, ..]
ones = repeat 1 -- [1,1,1,1,..] repeat ia un parametru, elementul/lista de vrei sa se repete

{-

intersperse 0 [1,2,3,4] -> [1,0,2,0,3,0,4]
intersperse '-' "Hello" -> "H-e-l-l-o"
zip naturals' ["w", "o"] -> [(0,"w"),(1,"o")]
evens = zipWith (+) naturals' naturals' - [0,2,4,6,8,..]
odds = zipWith (-) evens ones - [1,3,5,7,9,..]
fibo = 0 : 1 : 1 : zipWith (+) fibo (tail fibo)
concat [[1,2], [2,3]] -> [1,2,2,3] primeste ca parametru o lista de liste

"$"
length (tail (zip [1,2,3,4] ("abc" ++ "d"))) = length $ tail $ zip [1,2,3,4] $ "abc" ++ "d"

"." -> (f.g)(x) = f(g(x))
length . tail . zip [1,2,3,4] $ "abc" ++ "d"

f = (+1)
g = (*2)
f $ g $ 2 = f $ g 2 = f $ (2*2) = f $ 4 = f 4 = (4+1) = 5
f . g $ 2 = f(g(2)) = 5
-}

myIntersperse y = foldr (++) [] . map (: [y])

-----------------------------------------------------------------------------------------------
{-
    type Point = (Int,Int)
    p :: Point
    p = (2,3)
declarare + instantiere
-}

--data PointT = PointC Double Double deriving Show
data Point3D = Point3D Double Double Double deriving Show
data Colour = Red | Green | Blue | Black deriving Show

nonColour :: Colour -> Bool
nonColour Black = True
nonColour _ = False

data PointT = PointC {
    px :: Double,
    py :: Double
} deriving Show

data Maybe a = Just a | Nothing deriving (Show, Eq, Ord)
data List a = Void | Cons a (List a) deriving Show
data Natural = Zero | Succ Natural deriving Show
data Tree a = Tip | Node a (Tree a) (Tree a)

-- Polimorfism ad-hoc
elem' _ [] = False
elem' x (y : ys) = x == y || elem' x ys

-- Polimorfism parametric
length_ [] = 0
length_ (_:xs) = 1 + length_ xs

{-
    class  Eq a  where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
-}

{-
    data Person = Person {name :: String, cnp :: Integer}
    instance Eq Person where
        Person name1 cnp1 == Person2 name2 cnp2 = name1 == name2 && cnp1 == cnp2
        p1 /= p2 = not (p1 == p2)
-}

{-
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
    instance Functor [] where
        fmap = map
    instance Functor Maybe where
        fmap f (Just x) = Just (f x)
        fmap f Nothing = Nothing
-}

data Point = Point {
    x :: Integer,
    y :: Integer
}

instance Eq Point where
    Point x1 y1 == Point x2 y2 = x1 == x2 && y1 == y2 
    p1 /= p2 = not (p1 == p2)

class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

data NamedPoint = NamedPoint
    { pointName :: String
    , pointX :: Int
    , pointY :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p{ pointX = x
                            , pointY = y}

--instance MyClass [] where
  --  f = head      

--f x y = iterate $ x.y
--f :: (b -> a) -> (a -> b) -> a -> [a]
--iterate :: (d -> d) -> d -> [d]
--(.) :: (g -> h) -> (e -> g) -> (e -> h)

data Student = Student String [(String, Int, Int)] deriving (Eq, Show)
instance Ord Student where 
    Student _ stats1 <= Student _ stats2 = sum [credite | (_, nota, credite) <- stats1, nota >= 5] <= sum [credite | (_, nota, credite) <- stats2, nota >= 5]

--fu x = filter $ x . x . x
--x :: a, filter $ x . x . x :: b, f :: a -> b
--filter :: (c -> Bool) -> [c] -> [c]
--(.) :: (e -> g) -> (d -> e) -> (d -> g)
--fu :: (Bool -> Bool) -> [Bool] -> [Bool]

-- f u = map . u
-- (.) :: (b -> c) -> (a -> b) -> a -> c

--Solutie : f :: d -> e
-- u :: d = (a -> b)
-- map :: (g -> h) -> [g] -> [h]
-- b = g -> h
-- c = [g] -> [h]
-- e = a -> c
-- f :: (a -> g -> h) -> a -> [g] -> [h]

{-
instance (Num a, Ord a) => Ord [a] where
    xs <= ys = sum xs <= sum ys -- supraincarcare <= sa mearga si pe suma

instance Num Bool where
(+) = (||)
(*) = (&&) -- supraincarcare operatori sa functioneze pe si sau logic
-}

prefixes :: [a] -> [[a]]
prefixes s = out where
    out = [] : zipWith f out s
    f prefix x = prefix ++ [x]

{-
f g (x, y, z) = map g [x, y, z]
g = a -> b
map :: (c -> d) -> [c] -> [d]
x = e
y = f
z = g
f :: (a -> b) -> (a,a,a) -> [b]
-}

instance Num a => Num (Maybe a) where
    Just x + Just y = Just $ x + y
    _ + _ = Nothing