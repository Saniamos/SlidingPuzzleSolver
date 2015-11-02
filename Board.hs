module Board (
    Coor, Board (..), Move (..),
    new, reduceBoard, move, goMoves, counterMove, getEmptyCoor, getCellCoor, getCellValue, printMoves, checkBoards
) where

import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

--(x, y)
type Coor = (Int, Int)

--Brett, Länge oder Höhe, Position des leeren Feldes
data Board cell = Board (M.Map Coor cell) Int Coor deriving (Eq)

--Die vier Richtungen in die das leere Feld bewegt werden kann
data Move = U | R | D | L deriving (Show, Eq)

--Erzeugt ein Brett aus einer Liste, wobei das letzte Element dieser Liste die Representation des leeren Feldes sein muss
--Des Weiteren muss die Liste quadratische Länge haben
--Die Koordienaten des Brettes haben folgende Form:
--y/x 1 2 3 4 5 ..
--  1
--  2
--  3
--  .
--  .
new :: [a] -> Board a
new li
    | len^2 /= (length li) = Board M.empty 0 (1,1)
    | otherwise = Board (foldl (\b a -> M.insert (ind!!a) (li!!a) b) M.empty [0..(len^2 -1)]) len (len, len)
    where 
        len = (round $ sqrt (fromIntegral (length li)::Double))::Int
        ind = [(x,y) | y<-[1..len], x<-[1..len]]

--Gibt ein Brett ohne die oberste Reihe und linke Spalte zurück
reduceBoard :: Board a -> Board a
reduceBoard ma@(Board _ len _) = Board help (len-1) (len-1, len-1)
    where help = foldl (\b a@(x, y) -> M.insert (x-1, y-1) (getCellValue ma a) b) M.empty [(x,y) | x<-[2..len], y<-[2..len]]

--Tauscht zwei Zellen miteinander, wenn beide existieren
swapCells :: (Eq a) => Board a -> Coor -> Coor -> Board a
swapCells orig@(Board ma nI1 nI2) coor1 coor2 = if cellExists coor1 && cellExists coor2 then Board new nI1 nI2 else orig
    where
        cellExists = \x -> Nothing /= M.lookup x ma
        new = M.insert coor2 (fromJust $ M.lookup coor1 ma) (M.insert coor1 (fromJust $ M.lookup coor2 ma) ma)

--Bewegt das leere Feld in die angegebene Richtung
move :: (Eq a) => Board a -> Move -> Board a
move ma@(Board _ len coor@(x, y)) move = case move of
    U -> newBoard (x, y-1)
    R -> newBoard (x+1, y)
    D -> newBoard (x, y+1)
    L -> newBoard (x-1, y)
    where 
        newBoard = \co -> if (swapCells ma coor co) == ma then ma else newCoor (swapCells ma coor co) co
        newCoor = \(Board nI1 nI2 _) co -> Board nI1 nI2 co

--Gibt das Feld nach der Anwendung einer Liste von Bewegungen zurück
goMoves :: (Eq a) => Board a -> [Move] -> Board a
goMoves = foldl (\b a -> move b a)

--Gibt ein Feld vor der Anwendung einer Liste von Bewegungen zurück
--Nur für Debug Zwecke verwendent
goMovesBackward :: (Eq a) => Board a -> [Move] -> Board a
goMovesBackward ma li = foldl (\b a -> move b a) ma (map counterMove $ reverse li)

--Gibt den Zug zurück, welcher den gegebenen rückgängig macht
counterMove :: Move -> Move
counterMove move = case move of
    U -> D
    R -> L
    D -> U
    L -> R

--Gibt die Koordinaten des leeren Feldes zurück
--Erspart an einigen Stellen lamba Funktionen (bspw. Haskell:solveHelp)
getEmptyCoor :: Board a -> Coor
getEmptyCoor (Board _ _ coor) = coor

--Gibt die Koordinaten einer Zelle anhand ihres Inhalts wieder
getCellCoor :: (Eq a) => Board a -> a -> Coor
getCellCoor (Board ma len _) val = head $ filter (\h -> M.lookup h ma == Just val) [(x,y) | x<-[1..len], y<-[1..len]]

--Gibt den Wert einer Zelle zurück
getCellValue :: Board a -> Coor -> a
getCellValue (Board ma _ _) coor = fromJust $ M.lookup coor ma

--Gibt einen String mit jeweils einem Brett pro Zug zurück
printMoves :: (Eq a, Show a) => Board a -> [Move] -> String
printMoves board = snd . foldl (\(b, s) a -> (move b a, s ++ toString (move b a)) ) (board, "")

--Überprüft ob zwei Bretter die gleiche Länge und die gleichen Zellwerte haben
checkBoards :: (Show a, Eq a) => Board a -> Board a -> Bool
checkBoards (Board ma1 len1 _) (Board ma2 len2 _) = len1 == len2 && foldl (\b a -> b && (elem a $ M.elems ma1)) True (M.elems ma2)

--Represäntiert ein Brett als String
toString :: (Show a) => Board a -> String
toString ma@(Board bma len _) = border ++ (concat [lineString ma x cellwidth ++ border | x <- [1..len]])
    where 
        border = borderlineString len cellwidth
        cellwidth = lengthLongestElem ma

--Wandelt eien Zeile in einen String um
lineString :: (Show a) => Board a -> Int -> Int -> String
lineString (Board ma len _) y cellwidth = "|" ++ (" " ++ concat [ help x ++ (take (cellwidth - (length $ help x)) (cycle " ")) ++ " | " | x <- [1..len]]) ++ "\n"
    where 
        help = \x -> show $ fromJust (M.lookup (x, y) ma)

--Zeilenabgrenzung
borderlineString :: Int -> Int -> String
borderlineString line cell = (take (line*(cell+3)) $ cycle ("+" ++ (take (cell+2) $ cycle "-"))) ++ "+\n" --Plus 3 because "+" and the two spaces to sperate input from |

--Gibt die Länge des Längsten Zellenwertes zurück
lengthLongestElem :: (Show a) => Board a -> Int
lengthLongestElem (Board ma len _) = maximum $ map (\a -> length $ show $ fromJust $ M.lookup a ma) [(x,y) | x<-[1..len], y <-[1..len]]

--Show Instanz für ein Brett
instance (Show a) => Show (Board a) where  
    show board = toString board







