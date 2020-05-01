{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
 
{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}
 
data Directions = North | South | West | East
    deriving (Show, Eq, Ord)
 
{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}
 
type Position = (Int, Int)
 
{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = CellCon {
	getCh::Char,
	getPos::Position
} deriving (Eq, Ord, Show)
 
{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {
	getList::[[Cell]]
} deriving (Eq, Ord)
{-
    *** Optional *** 
 
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}
 
{-
    *** TODO ***
 
    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}
 
addEndl :: [Char] -> Int -> Int -> [Char]
addEndl [] _ _ = []
addEndl list cnt size = if cnt `mod` size == 0 
    then [head list] ++ [endl] ++ (addEndl (tail list) (cnt + 1) size) 
    else  [head list] ++ (addEndl (tail list) (cnt + 1) size)

    -- daca cnt = k * size -> sfarsit de linie ++ endl.


instance Show Level 
    where 
        show level = "\n" ++ (addEndl myCharList 1 size)
            where
                list = getList level -- scoatem [[Cell]] din level
                charList = [y | x <- list, y <- x] -- facem lista de [Cell]
                myCharList = map (\x -> getCh x) charList -- din [Cell] facem [Char]
                size = length $ head list -- lungimea oricarei liste din matrice o dam parametru lui myFunc
{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}
 
emptyLevel :: Position -> Level
emptyLevel (row, col) = Level (take (row + 1) $ repeat $ take (col + 1) $ repeat $ constr row col)
    where constr r c = CellCon emptySpace (r,c)
{-
    *** TODO ***
 
    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

verifGoodPosX :: Int -> Level -> Bool
verifGoodPosX x level 
    | x < (length $ getList level) && x >= 0 = True
    | otherwise = False

verifGoodPosY :: Int -> Level -> Bool
verifGoodPosY y level 
    | y < length (getList level !! 0) && y >= 0 = True
    | otherwise = False

addCell :: (Char, Position) -> Level -> Level
addCell (ch, (x,y)) level
    | verifGoodPosX x level && verifGoodPosY y level = (Level $ firstPartX ++ [firstPartY ++ [CellCon ch (x,y)] ++ (tail secondPartY)] ++ (tail secondPartX))
    | otherwise = level
        where
            list = getList level
            (firstPartX, secondPartX) = splitAt x $ list  
            (firstPartY, secondPartY) = splitAt y $ list !! x 
 
{-
    *** TODO *** 
 
    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe  
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel (row, col) myList = foldl (\ acc l -> (addCell l acc)) (emptyLevel (row, col)) myList
 
 
{-
    *** TODO ***
 
    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.
 
    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
verN :: Directions -> Bool
verN dir 
    | dir == North = True
    | otherwise = False

verS :: Directions -> Bool
verS dir 
    | dir == South = True
    | otherwise = False

verE :: Directions -> Int -> Int -> Bool
verE dir c s
    | dir == East && c + 1 < s = True
    | otherwise = False

verW :: Directions -> Bool
verW dir 
    | dir == West = True
    | otherwise = False

verifChar :: [[Cell]] -> Position -> Bool 
verifChar lvl (r,c) =
    if emptySpace == getCh(lvl !! r !! c) then True else False

moveCell :: Position -> Directions -> Level -> Level
moveCell (row, col) dir lvl
    | (elem chPos winningCells) || (elem chPos startCells) = lvl
    | verN dir && verifChar level (row - 1, col)          = (addCell cell (addCell (getCh levelPoz, (row - 1, col) ) lvl))
    | verS dir && verifChar level (row + 1, col)          = (addCell cell (addCell (getCh levelPoz, (row + 1, col) ) lvl))
    | verE dir col size && verifChar level (row, col + 1) = (addCell cell (addCell (getCh levelPoz, (row, col + 1) ) lvl))
    | verW dir && verifChar level (row, col - 1)          = (addCell cell (addCell (getCh levelPoz, (row, col - 1) ) lvl))
    | otherwise = lvl
    where 
        level = getList lvl
        levelPoz = level !! row !! col
        cell = (emptySpace, (row,col))
        chPos = getCh(level !! row !! col)
        size = length (level !! 0)

 
{-
    *** HELPER ***
 
    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.
 
    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection cell1 cell2 d =
	if getCh cell1 == topLeft && getCh cell2 == horPipe && d == East then True
	else if getCh cell1 == horPipe && getCh cell2 == topLeft && d == West then True
	else if getCh cell1 == botLeft && getCh cell2 == horPipe && d == East then True
	else if getCh cell1 == horPipe && getCh cell2 == botLeft && d == West then True
	else if getCh cell1 == winRight && getCh cell2 == horPipe && d == East then True
	else if getCh cell1 == horPipe && getCh cell2 == winRight && d == West then True
	else if getCh cell1 == topRight && getCh cell2 == horPipe && d == West then True
	else if getCh cell1 == horPipe && getCh cell2 == topRight && d == East then True
	else if getCh cell1 == botRight && getCh cell2 == horPipe && d == West then True
	else if getCh cell1 == horPipe && getCh cell2 == botRight && d == East then True
	else if getCh cell1 == startLeft && getCh cell2 == horPipe && d == East then True
	else if getCh cell1 == horPipe && getCh cell2 == startLeft && d == West then True
	else if getCh cell1 == startRight && getCh cell2 == horPipe && d == East then True
	else if getCh cell1 == horPipe && getCh cell2 == startRight && d == West then True
	else if getCh cell1 == winLeft && getCh cell2 == horPipe && d == West then True
	else if getCh cell1 == horPipe && getCh cell2 == winLeft && d == East then True
	else if getCh cell1 == topLeft && getCh cell2 == verPipe && d == South then True
	else if getCh cell1 == verPipe && getCh cell2 == topLeft && d == North then True
	else if getCh cell1 == topRight && getCh cell2 == verPipe && d == South then True
	else if getCh cell1 == verPipe && getCh cell2 == topRight && d == North then True
	else if getCh cell1 == botLeft && getCh cell2 == verPipe && d == North then True
	else if getCh cell1 == verPipe && getCh cell2 == botLeft && d == South then True
	else if getCh cell1 == botRight && getCh cell2 == verPipe && d == North then True
	else if getCh cell1 == verPipe && getCh cell2 == botRight && d == South then True
	else if getCh cell1 == startUp && getCh cell2 == verPipe && d == North then True
	else if getCh cell1 == verPipe && getCh cell2 == startUp && d == South then True
	else if getCh cell1 == startDown && getCh cell2 == verPipe && d == South then True
	else if getCh cell1 == verPipe && getCh cell2 == startDown && d == North then True
	else if getCh cell1 == winUp && getCh cell2 == verPipe && d == North then True
	else if getCh cell1 == verPipe && getCh cell2 == winUp && d == South then True
	else if getCh cell1 == winDown && getCh cell2 == verPipe && d == South then True
	else if getCh cell1 == verPipe && getCh cell2 == winDown && d == North then True
	else if getCh cell1 == botLeft && getCh cell2 == topLeft && d == North then True
	else if getCh cell1 == topLeft && getCh cell2 == botLeft && d == South then True
	else if getCh cell1 == botRight && getCh cell2 == topLeft && d == North then True
	else if getCh cell1 == topLeft && getCh cell2 == botRight && d == South then True
	else if getCh cell1 == topRight && getCh cell2 == topLeft && d == West then True
	else if getCh cell1 == topLeft && getCh cell2 == topRight && d == East then True
	else if getCh cell1 == startUp && getCh cell2 == topLeft && d == North then True
	else if getCh cell1 == topLeft && getCh cell2 == startUp && d == South then True
	else if getCh cell1 == startLeft && getCh cell2 == topLeft && d == West then True
	else if getCh cell1 == topLeft && getCh cell2 == startLeft && d == East then True
	else if getCh cell1 == winUp && getCh cell2 == topLeft && d == North then True
	else if getCh cell1 == topLeft && getCh cell2 == winUp && d == South then True
	else if getCh cell1 == winLeft && getCh cell2 == topLeft && d == West then True
	else if getCh cell1 == topLeft && getCh cell2 == winLeft && d == East then True
	else if getCh cell1 == botRight && getCh cell2 == botLeft && d == West then True
	else if getCh cell1 == botLeft && getCh cell2 == botRight && d == East then True
	else if getCh cell1 == topRight && getCh cell2 == botLeft && d == West then True
	else if getCh cell1 == botLeft && getCh cell2 == topRight && d == East then True
	else if getCh cell1 == startDown && getCh cell2 == botLeft && d == South then True
	else if getCh cell1 == botLeft && getCh cell2 == startDown && d == North then True
	else if getCh cell1 == startLeft && getCh cell2 == botLeft && d == West then True
	else if getCh cell1 == botLeft && getCh cell2 == startLeft && d == East then True
	else if getCh cell1 == winDown && getCh cell2 == botLeft && d == South then True
	else if getCh cell1 == botLeft && getCh cell2 == winDown && d == North then True
	else if getCh cell1 == winLeft && getCh cell2 == botLeft && d == West then True
	else if getCh cell1 == botLeft && getCh cell2 == winLeft && d == East then True
	else if getCh cell1 == topRight && getCh cell2 == botRight && d == South then True
	else if getCh cell1 == botRight && getCh cell2 == topRight && d == North then True
	else if getCh cell1 == startDown && getCh cell2 == botRight && d == South then True
	else if getCh cell1 == botRight && getCh cell2 == startDown && d == North then True
	else if getCh cell1 == startRight && getCh cell2 == botRight && d == East then True
	else if getCh cell1 == botRight && getCh cell2 == startRight && d == West then True
	else if getCh cell1 == winDown && getCh cell2 == botRight && d == South then True
	else if getCh cell1 == botRight && getCh cell2 == winDown && d == North then True
	else if getCh cell1 == winRight && getCh cell2 == botRight && d == East then True
	else if getCh cell1 == botRight && getCh cell2 == winRight && d == West then True
	else if getCh cell1 == startUp && getCh cell2 == topRight && d == North then True
	else if getCh cell1 == topRight && getCh cell2 == startUp && d == South then True
	else if getCh cell1 == startRight && getCh cell2 == topRight && d == East then True
	else if getCh cell1 == topRight && getCh cell2 == startRight && d == West then True
	else if getCh cell1 == winUp && getCh cell2 == topRight && d == North then True
	else if getCh cell1 == topRight && getCh cell2 == winUp && d == South then True
	else if getCh cell1 == winRight && getCh cell2 == topRight && d == East then True
	else if getCh cell1 == topRight && getCh cell2 == winRight && d == West then True
    else if getCh cell1 == horPipe && getCh cell2 == horPipe && d == West then True
    else if getCh cell1 == horPipe && getCh cell2 == horPipe && d == East then True
    else if getCh cell1 == verPipe && getCh cell2 == verPipe && d == North then True
    else if getCh cell1 == verPipe && getCh cell2 == verPipe && d == South then True 
	else False
 
{-
    *** TODO ***
 
    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

getStartCell :: [Cell] -> Cell
getStartCell list
    | elem (getCh ( head list )) startCells = head list 
    | otherwise = getStartCell (tail list) 

getWinCell :: [Cell] -> Cell
getWinCell list
    | elem (getCh ( head list )) winningCells = head list 
    | otherwise = getWinCell (tail list)  

verifWestConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifWestConnection (x,y) level  visited 
    | (connection ((level !! x) !! y) ((level !! x) !! (y - 1)) West && not (elem ((level !! x) !! (y - 1)) visited)) = True
    | otherwise = False

verifEastConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifEastConnection (x,y) level visited
    | (connection ((level !! x) !! y) ((level !! x) !! (y + 1)) East) && not (elem ((level !! x) !! (y + 1)) visited) = True
    | otherwise = False

verifSouthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifSouthConnection (x,y) level visited 
    | (connection ((level !! x) !! y) ((level !! (x + 1)) !! y) South) && not (elem ((level !! (x + 1)) !! y) visited) = True
    | otherwise = False

verifNorthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifNorthConnection (x,y) level visited 
    | (connection ((level !! x) !! y) ((level !! (x - 1)) !! y) North) && not (elem ((level !! (x - 1)) !! y) visited) = True
    | otherwise = False

checkWin :: Level -> Cell -> [Cell] -> Cell -> Bool
checkWin level cell visited winCell
        |(lvl !! x) !! y == winCell = True
	    | x < 0 || y < 0 = False -- out of bounds
	    | y - 1 >= 0 
            && verifWestConnection (x,y) lvl visited
		    = checkWin level ((lvl !! x) !! (y - 1)) (((lvl !! x) !! y) : visited) winCell
	    | y + 1 < (length (lvl !! 0)) 
            && verifEastConnection (x,y) lvl visited
		    = checkWin level ((lvl !! x) !! (y + 1)) ((lvl !! x) !! y : visited) winCell
	    | x + 1 < (length lvl) 
            && verifSouthConnection (x,y) lvl visited
		    = checkWin level (lvl !! (x + 1) !! y) ((lvl !! x) !! y : visited) winCell
	    | x - 1 >= 0 
            && verifNorthConnection (x,y) lvl visited
		    = checkWin level (lvl !! (x - 1) !! y) ((lvl !! x) !! y : visited) winCell
        | otherwise = False
            where
                (x,y) = getPos cell
                lvl = (getList level)

wonLevel :: Level -> Bool
wonLevel level = (checkWin level startCell [startCell] winCell)
    where 
        list = getList level -- [[Cell]]
        charList =  [y | x <- list, y <- x] -- [Cell]
        startCell = getStartCell charList -- starting point level-ului care vine
        winCell = getWinCell charList -- ending point level-ului
        -- recursiv sus/jos/stanga/dreapta in caz de oob 
        -- o pozitie ai sa nu te intorci

 
instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
 