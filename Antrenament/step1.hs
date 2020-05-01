doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = 
    if x < 100
        then x*2
        else x
doubleSmallNumber' x = (if x < 100 then x*2 else x) + 1
------------------------------

nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
------------------------------

-- [noun ++ " " ++ adjective | noun <- nouns, adjective <- adjectives]
------------------------------

length' xs = sum[1 | _ <- xs]
--------------------------------------

removeNonUpper st = [c | c <- st, c `elem` ['A'..'Z']]
--------------------------------------

listL = [[1,3,5,2,4] , [1,2,3,4,5,6,7], [1,4,9,8,5]]

vector = [[1,2,3], [3,4,5]]

-------------------------------------------

triangles = [ (a,b,c) | a <- [1..10], b <- [1..10] , c <- [1..10]]
right = [ (a,b,c) | c <- [1..10], b <- [1..c], a<- [1..b], a^2 + b^2 == c^2]




