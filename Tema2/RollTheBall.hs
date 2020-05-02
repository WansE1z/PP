{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
 
data Directions = North | South | West | East
    deriving (Show, Eq, Ord)
 
type Position = (Int, Int)

data Cell = CellCon {
	getCh::Char,
	getPos::Position
} deriving (Eq, Ord, Show)
 
data Level = Level {
	getMatrix::[[Cell]]
} deriving (Eq, Ord)
 
{-
    addEndl adauga cate un endl dupa fiecare sfarsit de linie
    daca contorul mod size == 0, inseamna ca este final de linie, deci adaugam endl
    daca cnt = k * size -> sfarsit de linie ++ endl.
-}

addEndl :: [Char] -> Int -> Int -> [Char]
addEndl [] _ _ = []
addEndl matrix cnt size = if cnt `mod` size == 0 
    then [head matrix] ++ "\n" ++ (addEndl (tail matrix) (cnt + 1) size) 
    else  [head matrix] ++ (addEndl (tail matrix) (cnt + 1) size)

{-
    la inceput de tot, trebuie adaugat un endl, dupa care apelez functia addEndl
-}

instance Show Level 
    where 
        show level = "\n" ++ (addEndl myCharList 1 size)
            where
                matrix = getMatrix level -- scoatem [[Cell]] din level
                charList = [ys | xs <- matrix, ys <- xs] -- facem lista de [Cell]
                myCharList = map (\xs -> getCh xs) charList -- din [Cell] facem [Char]
                size = length $ head matrix -- lungimea oricarei liste din matrice o dam parametru lui myFunc

{-
    inlocuieste fiecare element din matrice cu emptyspace
    repetand liniile, si coloanele, practic instantiez fiecare element cu emptyspace, folosind take, si creand level-ul
    daca avem liniile 0,1,2,3 , trebuie sa iau 4 linii, de aceea trebuie row + 1, la fel si pentru coloane
    take 4 repeat take 4  repeat emptyspace => o matrice 4x4 de emptyspace-uri.
-}
emptyLevel :: Position -> Level
emptyLevel pos = Level (take (row+1) $ repeat $ take (col+1) $ repeat $ constr row col)
    where
        row = fst pos
        col = snd pos 
        constr r c = CellCon emptySpace (r,c)

{-
    functia asta verifica ca pozitia sa fie valabila (sa nu fie out of bounds)
-}
verifGoodPos :: (Int,Int) -> Level -> Bool
verifGoodPos pos level 
    | (fst pos) < (length $ getMatrix level) && (fst pos) >= 0 && (snd pos) < length (getMatrix level !! 0) && (snd pos) >= 0 = True
    | otherwise = False

{-
    Pentru a adauga o celula la pozitia dorita, am facut aceasta functie de replace
    care functioneaza astfel : folosesc take sa iau elementele pana la pozitia de vreau sa modific
    dupa care dau append la valoarea noua, si adaug restul de elemente de urmau dupa elementul inlocuit
-}

replaceElement :: Int -> a -> [a] -> [a]
replaceElement pos newVal list = take pos list ++ newVal : drop (pos+1) list

{-
    Daca pozitia este valabila , atunci adaugam celula noua
    Fac o parcurgere in matrice pana la elementul dorit, acolo adaug celula noua , cu caracterul si pozitia dorita
    Ca sa o adaug, concatenez practic cele doua split-uri ale matricei 
    Ex :
    {
        0, 0, 0, 0
        0, 0, 0, 0
        0, 0, 0, 0
        0, 0, 0, 0
    }, unde 0 reprezinta emptySpace ->
    
    {
        0, 0, 0, 0
        0, 0, y, x
        x, x, x, x
        x, x, x, x
    }

    {   x, x, x, x
        x, x, y ,0
        0, 0, 0, 0
        0, 0, 0, 0
    }
    y = element adaugat nou, si x reprezinta elementele din split-ul celalalt.
    Datorita acestei abordari, dau replace la element unde trebuie, si dupa care fac concat intre cele doua split-uri.
-}

addCell :: (Char, Position) -> Level -> Level
addCell (ch, pos) level
    | verifGoodPos pos level = (Level (replaceElement (fst pos) cell matrix))
    | otherwise = level
        where
            matrix = getMatrix level
            (fstPart, sndPart) = splitAt (snd pos) $ matrix !! (fst pos)
            firstPart = concat [fstPart , [CellCon ch ((fst pos),(snd pos))]]
            secondPart = (tail sndPart)
            cell = concat [firstPart , secondPart]
 
{-
    Primind pozitia din dreapta jos a tablei, si lista de cells cu care trebuie umpluta matricea
    Parcurg de la dreapta la stanga, adaugand celulele, si returnand level-ul nou
    Adaug level-ul gol, peste care adaug celula noua.
-}

createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos cells = foldr (\ cell level -> (addCell cell level)) (emptyLevel (row, col)) cells
    where
        row = fst pos
        col = snd pos
-- ia elementul din lista, si adaug elementul din matrix in acumulator

{-
    verN/S/E/V - verifica daca directia este corecta
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

{-
    verfiChar este o functie auxiliara care verifica daca spatiul in care va urma sa se mute este gol, altfel, sa nu se mute celula
-}

verifChar :: [[Cell]] -> Position -> Bool 
verifChar lvl (r,c)
    | emptySpace == getCh(lvl !! r !! c) = True 
    | otherwise = False

{-
    Atunci cand se muta o celula, in locul respectiv din care s-a mutat trebuie pus un emptyspace
    De aceea, am implementat swap, care oarecum face asta, folosind de doua ori addCell, si returnand level-ul nou
-}

swap :: (Char, Position) -> Level -> (Char,Position) -> Level
swap (chPozLevel , (r,c)) level cell =
     addCell (chPozLevel, (r, c)) (addCell cell level)

{-
    daca caracterul de la randul si coloana respectiva face parte din lista de cell-uri de start sau win, atunci returnez direct level-ul (matricea)
    daca nu, verific directia in care urmeaza sa se mute, caracterul, si daca amandoua conditii returneaza true, fac swap-ul
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell pos dir lvl
    | (elem chPos winningCells) || (elem chPos startCells) = lvl
    | verN dir && verifChar level (row - 1, col)          = swap (getCh levelPoz, (row - 1, col)) lvl cell
    | verS dir && verifChar level (row + 1, col)          = swap (getCh levelPoz, (row + 1, col)) lvl cell
    | verE dir col size && verifChar level (row, col + 1) = swap (getCh levelPoz, (row, col + 1)) lvl cell
    | verW dir && verifChar level (row, col - 1)          = swap (getCh levelPoz, (row, col - 1)) lvl cell
    | otherwise = lvl
    where 
        row = fst pos
        col = snd pos
        level = getMatrix lvl
        levelPoz = level !! row !! col
        chPos = getCh(level !! row !! col)
        size = length (level !! 0)
        cell = (emptySpace, (row,col))

{-
    connection e o functie auxiliara in care am tratat toate cazurile posibile de conexiuni, pe directiile corespunzatoare
-}

connection :: Cell -> Cell -> Directions -> Bool
connection c1 c2 dir
    ----------------------------------- topLeft ------------------------------
    | getCh c1 == topLeft && getCh c2 == horPipe && dir   == East  ||
      getCh c1 == topLeft && getCh c2 == verPipe && dir   == South ||
      getCh c1 == topLeft && getCh c2 == botLeft && dir   == South ||
      getCh c1 == topLeft && getCh c2 == botRight && dir  == East  ||
      getCh c1 == topLeft && getCh c2 == topRight && dir  == East  ||
      getCh c1 == topLeft && getCh c2 == startUp && dir   == South ||
      getCh c1 == topLeft && getCh c2 == startLeft && dir == East  ||
      getCh c1 == topLeft && getCh c2 == winUp && dir     == South ||
      getCh c1 == topLeft && getCh c2 == winLeft && dir   == East = True
    ------------------------------------ topRight -----------------------------
    | getCh c1 == topRight && getCh c2 == horPipe && dir    == West  ||
      getCh c1 == topRight && getCh c2 == verPipe && dir    == South ||
      getCh c1 == topRight && getCh c2 == topLeft && dir    == West  ||
      getCh c1 == topRight && getCh c2 == botLeft && dir    == West  ||
      getCh c1 == topRight && getCh c2 == botRight && dir   == South ||
      getCh c1 == topRight && getCh c2 == startUp && dir    == South ||
      getCh c1 == topRight && getCh c2 == startRight && dir == West  ||
      getCh c1 == topRight && getCh c2 == winUp && dir      == South ||
      getCh c1 == topRight && getCh c2 == winRight && dir   == West  = True
    ------------------------------------- botLeft ------------------------------
    | getCh c1 == botLeft && getCh c2 == horPipe && dir    == East  ||
      getCh c1 == botLeft && getCh c2 == verPipe && dir    == North ||
      getCh c1 == botLeft && getCh c2 == topLeft && dir    == North ||
      getCh c1 == botLeft && getCh c2 == topRight && dir   == East  ||
      getCh c1 == botLeft && getCh c2 == botRight && dir   == East  ||
      getCh c1 == botLeft && getCh c2 == startDown && dir  == North ||
      getCh c1 == botLeft && getCh c2 == startLeft && dir  == East  ||
      getCh c1 == botLeft && getCh c2 == winDown && dir    == North ||
      getCh c1 == botLeft && getCh c2 == winLeft && dir    == East  = True
    -------------------------------------- botRight ----------------------------
    | getCh c1 == botRight && getCh c2 == horPipe && dir    == West  ||
      getCh c1 == botRight && getCh c2 == verPipe && dir    == North ||
      getCh c1 == botRight && getCh c2 == topLeft && dir    == West  ||
      getCh c1 == botRight && getCh c2 == topRight && dir   == North ||
      getCh c1 == botRight && getCh c2 == botLeft && dir    == West  ||
      getCh c1 == botRight && getCh c2 == startDown && dir  == North ||
      getCh c1 == botRight && getCh c2 == startRight && dir == West  ||
      getCh c1 == botRight && getCh c2 == winDown && dir    == North ||
      getCh c1 == botRight && getCh c2 == winRight && dir   == West  = True
    --------------------------------------- horPipe ----------------------------
    | getCh c1 == horPipe && getCh c2 == horPipe && dir     == West ||
      getCh c1 == horPipe && getCh c2 == horPipe && dir     == East ||
      getCh c1 == horPipe && getCh c2 == topLeft && dir     == West ||
      getCh c1 == horPipe && getCh c2 == topRight && dir    == East ||
      getCh c1 == horPipe && getCh c2 == botLeft && dir     == West ||
      getCh c1 == horPipe && getCh c2 == botRight && dir    == East ||
      getCh c1 == horPipe && getCh c2 == startLeft && dir   == West ||
      getCh c1 == horPipe && getCh c2 == startRight && dir  == West ||
      getCh c1 == horPipe && getCh c2 == winRight && dir    == West ||
      getCh c1 == horPipe && getCh c2 == winLeft && dir     == East = True
    --------------------------------------- verPipe ----------------------------
    | getCh c1 == verPipe && getCh c2 == verPipe && dir     == North ||
      getCh c1 == verPipe && getCh c2 == verPipe && dir     == South ||
      getCh c1 == verPipe && getCh c2 == topLeft && dir     == North ||
      getCh c1 == verPipe && getCh c2 == topRight && dir    == North ||
      getCh c1 == verPipe && getCh c2 == botLeft && dir     == South ||
      getCh c1 == verPipe && getCh c2 == botRight && dir    == South ||
      getCh c1 == verPipe && getCh c2 == startUp && dir     == South ||
      getCh c1 == verPipe && getCh c2 == startDown && dir   == North ||
      getCh c1 == verPipe && getCh c2 == winUp && dir       == South ||
      getCh c1 == verPipe && getCh c2 == winDown && dir     == North = True
    --------------------------------------- startUp -----------------------------
    | getCh c1 == startUp && getCh c2 == verPipe && dir     == North ||
      getCh c1 == startUp && getCh c2 == topLeft && dir     == North ||
      getCh c1 == startUp && getCh c2 == topRight && dir    == North = True
    --------------------------------------- startDown ---------------------------
    | getCh c1 == startDown && getCh c2 == verPipe && dir   == South ||
      getCh c1 == startDown && getCh c2 == botLeft && dir   == South ||
      getCh c1 == startDown && getCh c2 == botRight && dir  == South = True
    --------------------------------------- startLeft ---------------------------
    | getCh c1 == startLeft && getCh c2 == horPipe && dir   == West ||
      getCh c1 == startLeft && getCh c2 == topLeft && dir   == West ||
      getCh c1 == startLeft && getCh c2 == botLeft && dir   == West = True
    --------------------------------------- startRight --------------------------
    | getCh c1 == startRight && getCh c2 == horPipe && dir  == East ||
      getCh c1 == startRight && getCh c2 == botRight && dir == East ||
      getCh c1 == startRight && getCh c2 == topRight && dir == East = True
    --------------------------------------- winUp -------------------------------
    | getCh c1 == winUp && getCh c2 == verPipe && dir       == North ||
      getCh c1 == winUp && getCh c2 == topLeft && dir       == North ||
      getCh c1 == winUp && getCh c2 == topRight && dir      == North = True
    --------------------------------------- winDown -----------------------------
    | getCh c1 == winDown && getCh c2 == verPipe && dir     == South ||
      getCh c1 == winDown && getCh c2 == botLeft && dir     == South ||
      getCh c1 == winDown && getCh c2 == botRight && dir    == South = True
    --------------------------------------- winLeft -----------------------------
    | getCh c1 == winLeft && getCh c2 == horPipe && dir     == West ||
	  getCh c1 == winLeft && getCh c2 == topLeft && dir     == West ||
	  getCh c1 == winLeft && getCh c2 == botLeft && dir     == West = True
    --------------------------------------- winRight ----------------------------
    | getCh c1 == winRight && getCh c2 == horPipe && dir    == East ||
	  getCh c1 == winRight && getCh c2 == botRight && dir   == East ||
      getCh c1 == winRight && getCh c2 == topRight && dir   == East = True
    -----------------------------------------------------------------------------
    -- If there are no connections, return false
	| otherwise = False

{-
    functie de returneaza celula de start
    parcurg recursiv lista de cells, si daca gasesc elementul cautat in startCells, atunci il returnez, daca nu, continui recursivitatea
    asemanator si la getWinCell
-}

getStartCell :: [Cell] -> Cell
getStartCell matrix
    | elem (getCh ( head matrix )) startCells = head matrix 
    | otherwise = getStartCell (tail matrix) 

getWinCell :: [Cell] -> Cell
getWinCell matrix
    | elem (getCh ( head matrix )) winningCells = head matrix 
    | otherwise = getWinCell (tail matrix)  

{-
    urmatoarele verificari sunt pentru conexiunile pe fiecare directie
    de asemenea, verifica si ca pozitia respectiva sa nu mai fi fost vizitata
-}

verifNorthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifNorthConnection pos level visited 
    | (connection ((level !! row) !! col) ((level !! (row - 1)) !! col) North) && not (elem ((level !! (row - 1)) !! col) visited) = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

verifSouthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifSouthConnection pos level visited 
    | (connection ((level !! row) !! col) ((level !! (row + 1)) !! col) South) && not (elem ((level !! (row + 1)) !! col) visited) = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

verifEastConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifEastConnection pos level visited
    | (connection ((level !! row) !! col) ((level !! row) !! (col + 1)) East) && not (elem ((level !! row) !! (col + 1)) visited) = True
    | otherwise = False
        where
                row = fst pos
                col = snd pos

verifWestConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifWestConnection pos level  visited   
    | (connection ((level !! row) !! col) ((level !! row) !! (col - 1)) West && not (elem ((level !! row) !! (col - 1)) visited)) = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

{-
    checkWin este o functie auxiliara in care verific urmatoarele lucruri:
    - sa nu fie caz de out of bounds
    - daca se respecta conditiile pentru mutarea pozitiei la cele 4 pozitii cardinale, apelez recursiv functia si concatenez elementul care este verificat la lista visited
-}

checkWin :: Level -> Cell -> [Cell] -> Bool
checkWin level cell visited
        | matrix !! row !! col == winCell = True
	    | row < 0 || col < 0  = False -- out of bounds
        | row - 1 >= 0 
            && verifNorthConnection (row,col) matrix visited
		    = checkWin (moveCell (getPos cell) North level) (matrix !! (row - 1) !! col) (matrix !! row !! col : visited)
        | row + 1 < length matrix 
            && verifSouthConnection (row,col) matrix visited
		    = checkWin (moveCell (getPos cell) South level) (matrix !! (row + 1) !! col) (matrix !! row !! col : visited)
        | col + 1 < length (matrix !! 0) 
            && verifEastConnection (row,col) matrix visited
		    = checkWin (moveCell (getPos cell) East level) (matrix !! row !! (col + 1)) (matrix !! row !! col : visited)
	    | col - 1 >= 0 
            && verifWestConnection (row,col) matrix visited
		    = checkWin (moveCell (getPos cell) West level) (matrix !! row !! (col - 1)) (matrix !! row !! col : visited)
        | otherwise = False
            where
                (row,col) = getPos cell
                matrix = getMatrix level -- [[Cell]]
                charList =  [ys | xs <- matrix, ys <- xs] -- [Cell]
                winCell = getWinCell charList -- ending point level-ului

{-
    in wonLevel apelez functia auxiliara, dandu-i ca parametrii initiali matricea, celula de start si o lista in care adaug celula respectiva(visited-ul)
-}

wonLevel :: Level -> Bool
wonLevel level = (checkWin level startCell [startCell])
    where 
        matrix = getMatrix level -- [[Cell]]
        charList =  [col | row <- matrix, col <- row] -- [Cell]
        startCell = getStartCell charList -- starting point level-ului care vine

 
instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
 