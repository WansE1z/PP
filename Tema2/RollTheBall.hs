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
    getPos::Position,
	getCh::Char
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
        constr r c = CellCon (r,c) emptySpace

{-
    functia asta verifica ca pozitia sa fie valabila (sa nu fie out of bounds)
-}
verifGoodPos :: (Int,Int) -> Level -> Bool
verifGoodPos pos level 
    | (fst pos) < (length $ getMatrix level) && (fst pos) >= 0 && (snd pos) < length (getMatrix level !! 0) && (snd pos) >= 0 = True
    | otherwise = False

{-e
    Pentru a adauga o celula la pozitia dorita, am facut aceasta functie de replace
    care functioneaza astfel : folosesc take sa iau elementele pana la pozitia de vreau sa modific
    dupa care dau append la valoarea noua, si adaug restul de elemente de urmau dupa elementul inlocuit
-}

replaceElement :: Int -> a -> [a] -> [a]
replaceElement pos newVal list = take pos list ++ newVal : drop (pos + 1) list

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
    | verifGoodPos pos level = Level $ replaceElement (fst pos) cell matrix
    | otherwise = level
        where
            matrix = getMatrix level
            (fstPart, sndPart) = splitAt (snd pos) $ matrix !! fst pos
            firstPart = concat [fstPart , [CellCon (fst pos,snd pos) ch]]
            secondPart = tail sndPart
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
    | emptySpace == getCh (lvl !! r !! c) = True 
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
    | elem chPos startCells || elem chPos winningCells = lvl
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
        chPos = getCh levelPoz
        size = length $ level !! 0
        cell = (emptySpace, (row,col))

{-
    connection e o functie auxiliara in care am tratat toate cazurile posibile de conexiuni, pe directiile corespunzatoare
-}

connection :: Cell -> Cell -> Directions -> Bool
connection c1 c2 dir
    ----------------------------------- topLeft ------------------------------
    | ch_cell1 == topLeft && ch_cell2 == horPipe && dir   == East  ||
      ch_cell1 == topLeft && ch_cell2 == verPipe && dir   == South ||
      ch_cell1 == topLeft && ch_cell2 == botLeft && dir   == South ||
      ch_cell1 == topLeft && ch_cell2 == botRight && dir  == East  ||
      ch_cell1 == topLeft && ch_cell2 == topRight && dir  == East  ||
      ch_cell1 == topLeft && ch_cell2 == winUp && dir     == South ||
      ch_cell1 == topLeft && ch_cell2 == winLeft && dir   == East = True
    ------------------------------------ topRight -----------------------------
    | ch_cell1 == topRight && ch_cell2 == horPipe && dir    == West  ||
      ch_cell1 == topRight && ch_cell2 == verPipe && dir    == South ||
      ch_cell1 == topRight && ch_cell2 == topLeft && dir    == West  ||
      ch_cell1 == topRight && ch_cell2 == botLeft && dir    == West  ||
      ch_cell1 == topRight && ch_cell2 == botRight && dir   == South ||
      ch_cell1 == topRight && ch_cell2 == winUp && dir      == South ||
      ch_cell1 == topRight && ch_cell2 == winRight && dir   == West  = True
    ------------------------------------- botLeft ------------------------------
    | ch_cell1 == botLeft && ch_cell2 == horPipe && dir    == East  ||
      ch_cell1 == botLeft && ch_cell2 == verPipe && dir    == North ||
      ch_cell1 == botLeft && ch_cell2 == topLeft && dir    == North ||
      ch_cell1 == botLeft && ch_cell2 == topRight && dir   == East  ||
      ch_cell1 == botLeft && ch_cell2 == botRight && dir   == East  ||
      ch_cell1 == botLeft && ch_cell2 == winDown && dir    == North ||
      ch_cell1 == botLeft && ch_cell2 == winLeft && dir    == East  = True
    -------------------------------------- botRight ----------------------------
    | ch_cell1 == botRight && ch_cell2 == horPipe && dir    == West  ||
      ch_cell1 == botRight && ch_cell2 == verPipe && dir    == North ||
      ch_cell1 == botRight && ch_cell2 == topLeft && dir    == West  ||
      ch_cell1 == botRight && ch_cell2 == topRight && dir   == North ||
      ch_cell1 == botRight && ch_cell2 == botLeft && dir    == West  ||
      ch_cell1 == botRight && ch_cell2 == winDown && dir    == North ||
      ch_cell1 == botRight && ch_cell2 == winRight && dir   == West  = True
    --------------------------------------- horPipe ----------------------------
    | ch_cell1 == horPipe && ch_cell2 == horPipe && dir     == West ||
      ch_cell1 == horPipe && ch_cell2 == horPipe && dir     == East ||
      ch_cell1 == horPipe && ch_cell2 == topLeft && dir     == West ||
      ch_cell1 == horPipe && ch_cell2 == topRight && dir    == East ||
      ch_cell1 == horPipe && ch_cell2 == botLeft && dir     == West ||
      ch_cell1 == horPipe && ch_cell2 == botRight && dir    == East ||
      ch_cell1 == horPipe && ch_cell2 == winRight && dir    == West ||
      ch_cell1 == horPipe && ch_cell2 == winLeft && dir     == East = True
    --------------------------------------- verPipe ----------------------------
    | ch_cell1 == verPipe && ch_cell2 == verPipe && dir     == North ||
      ch_cell1 == verPipe && ch_cell2 == verPipe && dir     == South ||
      ch_cell1 == verPipe && ch_cell2 == topLeft && dir     == North ||
      ch_cell1 == verPipe && ch_cell2 == topRight && dir    == North ||
      ch_cell1 == verPipe && ch_cell2 == botLeft && dir     == South ||
      ch_cell1 == verPipe && ch_cell2 == botRight && dir    == South ||
      ch_cell1 == verPipe && ch_cell2 == winUp && dir       == South ||
      ch_cell1 == verPipe && ch_cell2 == winDown && dir     == North = True
    --------------------------------------- startUp -----------------------------
    | ch_cell1 == startUp && ch_cell2 == verPipe && dir     == North ||
      ch_cell1 == startUp && ch_cell2 == topLeft && dir     == North ||
      ch_cell1 == startUp && ch_cell2 == topRight && dir    == North = True
    --------------------------------------- startDown ---------------------------
    | ch_cell1 == startDown && ch_cell2 == verPipe && dir   == South ||
      ch_cell1 == startDown && ch_cell2 == botLeft && dir   == South ||
      ch_cell1 == startDown && ch_cell2 == botRight && dir  == South = True
    --------------------------------------- startLeft ---------------------------
    | ch_cell1 == startLeft && ch_cell2 == horPipe && dir   == West ||
      ch_cell1 == startLeft && ch_cell2 == topLeft && dir   == West ||
      ch_cell1 == startLeft && ch_cell2 == botLeft && dir   == West = True
    --------------------------------------- startRight --------------------------
    | ch_cell1 == startRight && ch_cell2 == horPipe && dir  == East ||
      ch_cell1 == startRight && ch_cell2 == botRight && dir == East ||
      ch_cell1 == startRight && ch_cell2 == topRight && dir == East = True
    -----------------------------------------------------------------------------
    | otherwise = False
    where
         ch_cell1 = getCh c1
         ch_cell2 = getCh c2

    -- Daca din conexiunile verificare, nu exista niciuna, inseamna ca trebuie returnat fals
    


{-
    functie de returneaza celula de start
    parcurg recursiv lista de cells, si daca gasesc elementul cautat in startCells, atunci il returnez, daca nu, continui recursivitatea
    asemanator si la getWinCell
-}

getStartCell :: [Cell] -> Cell
getStartCell matrix
    | elem (getCh $ head matrix) startCells = head matrix 
    | otherwise = getStartCell $ tail matrix 

getWinCell :: [Cell] -> Cell
getWinCell matrix
    | elem (getCh $ head matrix) winningCells = head matrix 
    | otherwise = getWinCell $ tail matrix  

{-
    urmatoarele verificari sunt pentru conexiunile pe fiecare directie
    de asemenea, verifica si ca pozitia respectiva sa nu mai fi fost vizitata
-}

verifConnection :: Position -> [[Cell]] -> [Cell] -> Directions -> Bool
verifConnection pos level visited dir
    | dir == North = verifNorthConnection pos level visited
    | dir == South = verifSouthConnection pos level visited
    | dir == East = verifEastConnection pos level visited
    | dir == West = verifWestConnection pos level visited
    | otherwise = False

verifNorthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifNorthConnection pos level visited 
    | row - 1 >= 0 
            && connection (level !! row !! col) (level !! (row - 1) !! col) North 
            && notElem (level !! (row - 1) !! col) visited = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

verifSouthConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifSouthConnection pos level visited 
    | row + 1 < length level 
            && connection (level !! row !! col) (level !! (row + 1) !! col) South 
            && notElem (level !! (row + 1) !! col) visited = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

verifEastConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifEastConnection pos level visited
    | col + 1 < length (level !! 0) 
        && connection (level !! row !! col) (level !! row !! (col + 1)) East 
        && notElem (level !! row !! (col + 1)) visited = True
    | otherwise = False
        where
            row = fst pos
            col = snd pos

verifWestConnection :: Position -> [[Cell]] -> [Cell] -> Bool
verifWestConnection pos level visited   
    | col - 1 >= 0 
        && connection (level !! row !! col) (level !! row !! (col - 1)) West 
        && notElem (level !! row !! (col - 1)) visited = True
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
        | verifConnection (row,col) matrix visited North
		    = checkWin (moveCell (getPos cell) North level) (matrix !! (row - 1) !! col) addVisit
        | verifConnection (row,col) matrix visited South
		    = checkWin (moveCell (getPos cell) South level) (matrix !! (row + 1) !! col) addVisit
        | verifConnection (row,col) matrix visited East
		    = checkWin (moveCell (getPos cell) East level) (matrix !! row !! (col + 1)) addVisit
	    | verifConnection (row,col) matrix visited West
		    = checkWin (moveCell (getPos cell) West level) (matrix !! row !! (col - 1)) addVisit
        | otherwise = False
            where
                (row,col) = getPos cell
                matrix = getMatrix level -- [[Cell]]
                charList =  [ys | xs <- matrix, ys <- xs] -- [Cell]
                winCell = getWinCell charList -- ending point level-ului
                addVisit = visited ++ [matrix !! row !! col]

{-
    in wonLevel apelez functia auxiliara, dandu-i ca parametrii initiali matricea, celula de start si o lista in care adaug celula respectiva(visited-ul)
-}

wonLevel :: Level -> Bool
wonLevel level = checkWin level startCell [startCell]
    where 
        matrix = getMatrix level -- [[Cell]]
        charList =  [col | row <- matrix, col <- row] -- [Cell]
        startCell = getStartCell charList -- starting point level-ului care vine

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
 