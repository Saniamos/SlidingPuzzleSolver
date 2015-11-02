module Haskell (
    solve, generate, generateNum, solveGenNum, solveGenWord, buildBoards
)where

import Board
import System.Random
--http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
import Data.List.Split

--Build Puzzle from String
-- ***********************************************************************************************************
--Erzeugt aus einem String zwei Bretter, wobei davon ausgegangen wird, dass das erste das ungelöste und das zweite das gelöste Brett ist
buildBoards :: String -> IO (Board String, Board String)
buildBoards str = if length boards /= 2 then error "The file should contain two boards" else
    if checkBoards unsolved solved then return (unsolved, solved) else error "The given boards either are not of same length or have different symbols in them"
    where
        boards = splitOn "\n" str
        unsolved = new $ splitOn "," $ boards!!0
        solved = new $ splitOn "," $ boards!!1

--Solve generated puzzles
-- ***********************************************************************************************************
--Löst ein generiertes Puzzle, mit der gegebenen Länge und Zahl an Schritten
solveGenNum :: Int -> Int -> IO (Board String, Board String, Bool, [Move])
solveGenNum boardLen movLen = do
    boards <- generateNum boardLen movLen
    let solved = snd boards
    let unsolved = fst boards
    let moves = solve unsolved solved
    return (unsolved, goMoves unsolved moves, (goMoves unsolved moves == solved), moves)

--Löst ein gereriertes Wörterpuzzle
solveGenWord :: Int -> Int -> IO (Board Char, Board Char, Bool, [Move])
solveGenWord boardLen movLen = do
    boards <- generate boardLen movLen
    let solved = snd boards
    let unsolved = fst boards
    let moves = solve unsolved solved
    return (unsolved, goMoves unsolved moves, (goMoves unsolved moves == solved), moves)


--Filter
-- ***********************************************************************************************************
--Filtert alle Züge, die sich gegenseitig aufheben um die Ausgabe kürzer zu gestalten
filterMoves :: [Move] -> [Move]
filterMoves mv | filterMovesHelp mv [] == mv = mv
filterMoves mv = filterMoves $ filterMovesHelp mv []

--Hilfsfunktion zu filterMoves
filterMovesHelp :: [Move] -> [Move] -> [Move]
filterMovesHelp mv li 
    | length mv == 0 = li
    | length mv == 1 = li ++ mv
filterMovesHelp (x:xs) li = if x == counterMove (head xs) then filterMovesHelp (tail xs) li else filterMovesHelp xs (li ++ [x])

--Generate
-- ***********************************************************************************************************
--Generiert ein Puzzle mit Zahlen. Da ein leeres Element angegeben werden muss und sich die 0 recht schlecht liest, wandeln wir jede Zahl in einen String um
generateNum :: Int -> Int -> IO (Board String, Board String)
generateNum boardLen movLen = do
    let word = (map (\a -> show a) [1..((boardLen^2)-1)]) ++ [" "]
    let solved = new word
    unsolved <- randomSteps solved movLen
    return (unsolved, solved)

--Generiert ein Puzzle, mit der gegebenen Brettlänge, aus einem zufällig gewählten Wort
generate :: Int -> Int -> IO (Board Char, Board Char)
generate boardLen movLen = do
    word <- chooseWord boardLen
    let solved = new word
    unsolved <- randomSteps solved movLen
    return (unsolved, solved)

--Liest die Wörter aus dem englischen Scrabble ein
--https://scrabblehelper.googlecode.com/svn-history/r20/trunk/ScrabbleHelper/src/dictionaries/sowpods.txt
getWords :: IO ([[Char]])
getWords = do 
    wordsFromDic <- readFile "sowpods.txt"
    return $ splitOn "\r\n" wordsFromDic --Sollte auch mit lines funktionieren

--Wählt zufällig ein Wort mit der Länge (Brettlänge*Brettlänge-1)
chooseWord :: Int -> IO ([Char])
chooseWord boardLen = do
    wor <- getWords
    let help = filter (\h -> length h == boardLen^(2::Int)-1) wor
    randWord <- pickRandom help
    return $ randWord ++ [' ']

--Geht zufällige Schritte auf dem Brett, wobei darauf geachtet, wird das zum Schluss das leere Feld wieder unten rechts ist
randomSteps :: (Show a, Eq a) => Board a -> Int -> IO (Board a)
randomSteps ma@(Board _ len _) movLen = do
    let initial = randomStepsInitial (return ma) (div movLen 2) movLen
    operatable <- initial
    moveEmptyBack operatable

--Bringt das leere Feld mit zufälligen Schritten wieder nach unten rechts
moveEmptyBack :: (Eq a) => Board a -> IO (Board a)
moveEmptyBack board = do
    let stepsBa = stepsBack board
    foldl (\board a -> do 
            b <- board
            randomStep b (possibleMoves b [U,L]))
        (return board) [1..stepsBa]

--Der Anfang der zufälligen Schritte, bevor das leere Feld zurück gebracht werden muss
randomStepsInitial :: (Eq a) => IO (Board a) -> Int -> Int -> IO (Board a)
randomStepsInitial board 0 _ = board
randomStepsInitial startBoard iterations movLenTemp = do
    operatable <- randomStepsHelp startBoard iterations
    let stepsBa = stepsBack operatable
    randomStepsInitial (return operatable) (div (movLenTemp - iterations - stepsBa) 2) (movLenTemp - iterations)

--Gibt die Anzahl an Schritten zurück, die benötigt wird um das leere Feld an den Ausgangspunkt zu bringen
stepsBack :: Board a -> Int
stepsBack (Board _ len (x, y)) = len - x + len - y

--Die eigentlichen Schritte
randomStepsHelp :: (Eq a) => IO (Board a) -> Int -> IO (Board a)
randomStepsHelp startBoard iterations =
    foldl (\board a -> do 
        b <- board
        randomStep b (possibleMoves b [])) 
    startBoard [1..iterations]

--Gibt die möglichen Züge ohne die Übergebenen zurück
possibleMoves :: Board a -> [Move] -> [Move]
possibleMoves (Board _ len (x, y)) li = filter (\h -> not $ elem h li) (up ++ right ++ down ++ left)
    where
        up    = if y > 1    then [U] else []
        right = if x < len  then [R] else []
        down  = if y < len  then [D] else []
        left  = if x > 1    then [L] else []

--Wählt einen zufälligen Zug aus den Übergebenen aus und gibt das Brett zurück
randomStep :: (Eq a) => Board a -> [Move] -> IO (Board a)
randomStep ma moves = do 
    chosenMove <- pickRandom moves
    return $ move ma chosenMove

--Funktion aus der VL, wählt ein zufälliges Element einer Liste
pickRandom :: [a] -> IO a
pickRandom [] = error "There is no word with so many Chars" 
pickRandom xs = do
    i <- randomRIO (0, length xs - 1)
    return $ xs !! i

--Solve
-- ***********************************************************************************************************
--Löst ein übergebenes Puzzle und filtert das Ergebnis, wobei der erste Parameter als ungelöstest und der zweite als gelöstes Puzzle interpretiert wird
solve :: (Eq a, Show a) => Board a -> Board a -> [Move]
solve ma1 ma2 = if checkBoards ma1 ma2 then filterMoves $ solveHelp ma1 ma2 else error "The given boards either are not of same length or have different symbols in them"

--Löst ein übergebenes Puzzle, wobei der erste Parameter als ungelöstest und der zweite als gelöstes Puzzle interpretiert wird
solveHelp :: (Eq a, Show a) => Board a -> Board a -> [Move]
solveHelp ma1@(Board _ len _) ma2@(Board _ len2 _)
    | len /= len2 = []
    | len == 1 = []
    | len == 2 = len2Help ++ (if (fst $ getEmptyCoor $ goMoves ma1 len2Help) == 2 then [D] else [R])
    | otherwise = snd outerSorted ++ solveHelp (reduceBoard $ fst outerSorted) (reduceBoard ma2)
    where
        outerSorted = solveOuter ma1 ma2
        getStartCoor = \board coor -> getCellCoor board (getCellValue ma2 coor)
        len2Help = moveCellDirectly ma1 (getStartCoor ma1 (1,1)) (1,1)

--Löst obere Reihe und linke Spalte
solveOuter :: (Eq a, Show a) => Board a -> Board a -> (Board a, [Move])
solveOuter ma1@(Board _ len _) ma2 = (goMoves (fst leftSorted) finishedMoves, (snd topSorted) ++ (snd leftSorted) ++ finishedMoves)
    where 
        topSorted = solveRow ma1 ma2 True
        leftSorted = solveRow (fst topSorted) ma2 False
        finishedMoves = moveEmpty (fst leftSorted) (len, len)

--Löste die obere Reihe, wenn True übergeben wird und die linke Spalte bei False
--Zunächst werden hierfür alle Zellen, die nicht in die obere linke Ecke gehören angeordnet und anschließend die Ecke eingefügt
--Dabei wird bei der Reihe auf folgendes geachtet:
--1. Wurde eine Zelle richtig angeordnet und das leere Feld befindet sich auf der ersten Reihe, so wird es eine Feld nach unten bewegt
--Bei einer Spalte:
--1. Ist die Zelle, die neu angeordnet werden soll, über ihrem neuen Platz so wird sie erst auf der y-Achse bewegt, bevor der Standardablauf weiter läuft,
--  damit die Funktion moveCellDirectly nicht schon angeordnete Zellen verschiebt
--2. Wurde eine Zelle richtig angeordnet und das leere Feld befindet sich auf der linken Spalte, so wird es ein Feld nach rechts bewegt
solveRow :: (Eq a, Show a) => Board a -> Board a -> Bool -> (Board a, [Move])
solveRow ma1@(Board _ len _) ma2 row = (goMoves (fst withoutEdge) withEdgeMoves, (snd withoutEdge) ++ withEdgeMoves)
    where
        corner = if row then (len, 1) else (1, len)
        edge = \a -> if row then (a,1) else (1,a)
        getStartCoor = \board coor -> getCellCoor board (getCellValue ma2 coor)
        withoutEdge = foldl (\b a -> let 
            startcoor = getStartCoor (fst b) (edge a)
            moveshelp = if (not row) && (snd startcoor < a) then moveCellDirectly (fst b) startcoor (fst startcoor, a) else []
            moves = moveshelp ++ moveCellDirectly (goMoves (fst b) moveshelp) (getStartCoor (goMoves (fst b) moveshelp) (edge a)) (edge a)
            catchRowMess = if snd (getEmptyCoor (goMoves (fst b) moves))==1 then [D] else []
            catchColMess = if fst (getEmptyCoor (goMoves (fst b) moves))==1 then [R] else []
            finishedMoves = if startcoor == (edge a) then [] else moves ++ (if row then catchRowMess else catchColMess)
            in
                (goMoves (fst b) finishedMoves, snd b ++ finishedMoves)) (ma1, []) [1..(len-1)]
        withEdgeMoves = if getStartCoor (fst withoutEdge) corner == corner then [] else moveCellCorner (fst withoutEdge) (getStartCoor (fst withoutEdge) corner) corner

--Bewegt eine Zelle von der ersten Koordinate zur zweiten, wobei davon ausgegangen wird, dass die zweite eine Ecke ist
--Hierfür wird die zu bewegende Zelle zwei Zellen vor ihr Ziel gebracht und dann eine Standardabfolge aufgerufen, die sicherstellt, dass das benachbarte Feld der Reihe bzw. Spalte an seinem Platz bleibt
moveCellCorner :: (Show a, Eq a) => Board a -> Coor -> Coor -> [Move]
moveCellCorner ma@(Board _ len coor) coor1 (x2, y2) = cellMovment ++ cellRightHelp ++ moveCellRoatateUppercorner upper
    where
        upper = x2 == len
        coorHelp = if upper then (x2, y2+2) else (x2+2, y2)
        cellMovment = moveCellDirectly ma coor1 coorHelp
        cellRightHelp = moveCellCornerEmptyRight (goMoves ma cellMovment) coorHelp

--Bewegt das leere Feld an die Anfangsposition der Abfolge,
--Bei Reihen also hierhin:
-- 1 2 4
-- 6   8
-- 7 5 3
--Bei Spalten ist das ganze lediglich gedreht
moveCellCornerEmptyRight :: (Show a) => Board a -> Coor -> [Move]
moveCellCornerEmptyRight ma@(Board _ len (x, y)) (x1, y1)
    | x == len && (y1+1) == y = [L,U,U]
    | y == len && (x1+1) == x = [U,L,L]
    | x == (x1-1) = [U]
    | y == (y1-1) = [L]
 
--Abfolge um bei der erzeugten Position die Zelle in die Ecke zu bewegen
moveCellRoatateUppercorner :: Bool -> [Move]
moveCellRoatateUppercorner True = [U,R,D,L,U,R,D,D,L,U,U,R,D]
moveCellRoatateUppercorner _ =    [L,D,R,U,L,D,R,R,U,L,L,D,R]

--Bewegt eine Zelle von der ersten zur zweiten Koordinate
--Dazu wird das leere Feld zunächst in Bewegungsrichtung an die Seite der zu bewegenden Zelle gebracht
--Anschließend wird die Zelle auf der X-Achse bis zu ihrer x-Koordinate bewegt
--Daraufhin wird das leere Feld in Bewegungsrichtung über oder unter die zu bewegende Zelle gebracht
--Und abschließend wird die Zelle auf der y-Achse an die richtige Koordinate gebracht
--Aus diesem Grund müssen in solveRow einige Fälle modifiziert wird
--Ist die Zelle schon auf einer Achse an der richtigen Stelle, so werden die entsprechenden Schritte nicht zurück gegeben
moveCellDirectly :: (Eq a) => Board a -> Coor -> Coor -> [Move]
moveCellDirectly ma@(Board _ len _) coor1@(x1, y1) coor2@(x2, y2) = step1 ++ (if x1 /= x2 then stepsForX else []) ++ (if stepsForY /= [] then step2 ++ stepsForY else [])
    where
        mov1 = if y1 == len then U else D --Bewege das leere Feld immer unter der Zelle durch, es sei denn es gibt keine Reihe unter der Zelle
        mov2 = if x2 == len then L else R --Bewege das leere Feld immer rechts an der Zelle vorbei, es sei denn es gibt keine Spalte rechts der Zelle
        step1 = moveEmptyLRTo ma coor1 coor2 mov1
        stepsForX = moveCellXYRight coor1 coor2 mov1
        boardAfterX = goMoves ma (step1 ++ stepsForX)
        step2 = moveEmptyUDTo boardAfterX coor2
        stepsForY = moveCellXYRight coor1 coor2 mov2

--Bewegt das leere Feld an die linke oder rechte Seite des zu bewegenden Feldes (erste Koordinate), je nachdem, in welche Richtung letzteres bewegt werden soll
--Dabei wird darauf geachtet, wie sich die drei Koordinaten (leeres Feld, start Feld, end Feld) zu eineander verhalten um keine unerwünschten Verschiebungen der Werte zu haben
moveEmptyLRTo :: (Eq a) => Board a -> Coor -> Coor -> Move -> [Move]
moveEmptyLRTo board@(Board _ len (x, _)) (x1, y1) (x2, _) mov1 = 
    if emptyLeftOf == goalLeftOf then
        moveEmpty board (if goalLeftOf then x1-1 else x1+1 , y1)
    else 
        moveEmpty board (if goalLeftOf then x1+1 else x1-1, y1) ++ (if x1 == len then [] else (moveEmptyAroundCell (if goalLeftOf then L else R) mov1))
    where 
        emptyLeftOf = x < x1 || x1 == len
        goalLeftOf = x2 < x1 || (x2 == len && x1 == len)

--Bewegt das leere Feld über ode runter das zu bewegende Feld, wobei davon ausgegangen wir, dass es sich neben letzterem befindet
--Dabei wird darauf geachtet nicht schon richtig angeordneten Felder wieder zu verschieben. Konkret meint das den Fall, wenn ein Feld in der oberen Reihe eingeordnet wird
moveEmptyUDTo :: (Eq a) => Board a -> Coor -> [Move]
moveEmptyUDTo (Board _ len (x, y)) (x1, y1) = if y == 2 && x1 /= 1 && y1 == 1 && x < x1 then [D,R,R,U,U,L] else [if y < y1 then D else U, if x < x1 then R else L]

--Bewegt eine Zelle auf der x- oder y-Achse an die richtige Position, je nachdem wie der gegebene Move zulässt, dass das leere um das zu bewegende Feld herumgeführt wird
--Es ergibt z.B. keinen Sinn das leere Feld rechts um das zu bewegende Feld herum zu bewegen, wenn es nach rechts verschoben werden soll.
moveCellXYRight :: Coor -> Coor -> Move -> [Move]
moveCellXYRight (x1, y1) (x2, y2) mov 
    | elem mov [U, D] && x1 < x2 = new L (x2 - x1) -- auf der x-Achse
    | elem mov [U, D] && x1 > x2 = new R (x1 - x2)
    | elem mov [L, R] && y1 < y2 = new U (y2 - y1) -- auf der y-Achse
    | elem mov [L, R] && y1 > y2 = new D (y1 - y2)
    | otherwise = []
    where
        new = \dir n -> foldl (\li _ -> li ++ [dir] ++ (moveEmptyAroundCell (counterMove dir) mov)) [] [2..n] ++ [dir]

--Geht immer zuerst auf der x-Achse zur richtigen Koordinate und dann auf der y-Achse,
--Wenn die Koordinaten gleich sind muss nichts bewegt werden und es wird [] zurück gegeben
moveEmpty :: (Eq a) => Board a -> Coor -> [Move]
moveEmpty ma@(Board _ _ coor) coor1 = if coor == coor1 then [] else snd $ moveEmptyHelp (ma, []) coor1

--Hilfsfunktion, die sich selbst rekursiv aufruft, bis x- und y-Koordinate korrekt sind
moveEmptyHelp :: (Eq a) => (Board a, [Move]) -> Coor -> (Board a, [Move])
moveEmptyHelp (ma@(Board _ _ (x, y)), li) co@(x2, y2)
    | x < x2 = moveEmptyHelp (new R (x2-x)) co
    | x > x2 = moveEmptyHelp (new L (x-x2)) co
    | y < y2 = moveEmptyHelp (new D (y2-y)) co
    | y > y2 = moveEmptyHelp (new U (y-y2)) co
    | otherwise = (ma, li)
    where new = \dir n -> foldl (\(b, li) a -> (move b dir, li ++ [dir])) (ma, li) [1..n]

--Bewegt das leere Feld in die als ersten Parameter übergebene Richtung.
--Damit das erste Feld in dieser Richtung nicht seine Position verändert, wird das leere Feld mit der zweiten Richtung um selbiges herum geführt
moveEmptyAroundCell :: Move -> Move -> [Move]
moveEmptyAroundCell findir dir = [dir, findir, findir, counterMove dir]





