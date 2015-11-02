module Main where

import Board
import Haskell
import System.Environment (getArgs)

--Die Hauptschleife des Programmes
--Wir haben hier bewusst darauf verzichtet, unsere solveGenericWord zu verwenden, da das größte Brett dort nur eine Länge von 4 haben kann und in dem entsprechenden 15 Zeichen langen Wort Buchstaben mehrfach vorkommen können. Bei diesen Puzzles ist eine Lösung nicht garantiert.
main = do
    args <- getArgs
    case args!!0 of
        "-p" -> --wenn der Spieler das Brett selbst spielen möchte
            if length args == 2 then do --läd das Brett aus der angegebenen Datei
                fileCont <- readFile (args!!1)
                boards <- buildBoards fileCont
                play (fst boards) (snd boards)
            else do --erzeugt ein Brett mit den gegebenen Parametern
                boards <- generateNum (read $ args!!1) (read $ args!!2) 
                play (fst boards) (snd boards)
        "-m" -> --wenn alle Züge als einzelnes Brett ausgegeben werden sollen
            if length args == 2 then do --läd das Brett aus der angegebenen Datei
                fileCont <- readFile (args!!1)
                boards <- buildBoards fileCont
                let moves = solve (fst boards) (snd boards)
                putStrLn $ printMoves (fst boards) $ solve (fst boards) (snd boards)
            else do --erzeugt ein Brett mit den gegebenen Parametern
                boards <- generateNum (read $ args!!1) (read $ args!!2)
                putStrLn $ printMoves (fst boards) $ solve (fst boards) (snd boards)
        otherwise -> do
            if length args == 1 then do --löst das Brett aus der Datei
                fileCont <- readFile (args!!0)
                boards <- buildBoards fileCont
                putStrLn $ (show $ fst boards) ++ (show $ goMoves (fst boards) $ solve (fst boards) (snd boards))
            else do --löst ein nummersches Puzzle mit den gegebenen Parametern
                help <- solveGenNum (read $ args!!0) (read $ args!!1)
                putStrLn $ show help

--Schleife zum Spielen eines Brettes
--Das leere Feld wird mit wasd verschoben, Hilfe wird bei h angezeigt und e zeigt die Lösung eines Brettes
play board solved = do
    putStr $ show board
    if board == solved then do
        putStrLn "congrats you solved it!" 
    else do
        m <- getLine
        case m of
            "w" -> play (move board U) solved
            "d" -> play (move board R) solved
            "s" -> play (move board D) solved
            "a" -> play (move board L) solved
            "h" -> do
                putStrLn "Move the empty field with wasd. To show solution type e"
                play board solved
            "e" -> do
                putStrLn (show solved)
                play board solved
            otherwise -> do
                putStrLn "are you sure (y/n) "
                answer <- getLine
                if answer == "y" then 
                    putStrLn "\nthats sad"
                else do
                    putStrLn "okay lets go on"
                    play board solved




