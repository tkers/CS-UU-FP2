module Main where

import Data.Array
import Data.List
import Parse
import Types
import Rules
import Show
import Predict
import System.IO
import System.Environment

-- usage: main <board file path> <lookahead depth>
-- lookahead depth is defined as the number of turns the current player must make to win
main :: IO ()
main = do
        args <- getArgs
        s <- readFile (args !! 0) -- first argument for board file
        let sit = parse s -- parse board file
            depth = 1 + ((read (args !! 1)) - 1) * 2
        gameLoop chessRules sit (predict chessRules sit) depth True
        return ()

-- takes ruleset, current situation, predicted outcomes, prediction depth, redraw flag
gameLoop :: Rules -> Situation -> MoveTree (Situation, Bool) -> Int -> Bool -> IO ()
gameLoop r@(Rules lost moves move) s@(Situation b@(Board a) curr) m@(MoveTree sit list) depth redraw = do
                                                                                                        if not redraw -- don't redraw the board if player gave invalid input
                                                                                                            then putStrLn ""
                                                                                                            else do putStrLn "" >> print b -- redraw board
                                                                                                                    if not (hasLost)
                                                                                                                        then do let pn = (predictLoss' lost depth m) -- number of turns after which current player wins
                                                                                                                                if pn <= depth -- check if possible win was found
                                                                                                                                    then putStrLn (show curr ++ " can win in " ++ show ((pn - 1) `div` 2 + 1) ++ " turns!")
                                                                                                                                    else putStrLn ""
                                                                                                                        else putStrLn ""
                                                                                                        if hasLost
                                                                                                            then do putStrLn (show (if curr == White then Black else White) ++ " wins!")
                                                                                                                    return () -- end game
                                                                                                            else do putStrLn ((show curr) ++ "'s move:")
                                                                                                                    moveInp <- getLine
                                                                                                                    if not (tryParse moveInp) -- catch invalid input
                                                                                                                        then do putStrLn "Invalid input."
                                                                                                                                retry -- loop on same situation
                                                                                                                        else do
                                                                                                                                let parsedMove = parseMove moveInp -- parse input to positions
                                                                                                                                    from = fst parsedMove
                                                                                                                                    from' = a ! from -- piece at move's origin
                                                                                                                                    dest = snd parsedMove
                                                                                                                                if from' == EmptyPiece
                                                                                                                                    then do putStrLn "Invalid start of move."
                                                                                                                                            retry
                                                                                                                                    else if ((\ (Piece c _) -> c) from') /= curr -- check ownership
                                                                                                                                            then do putStrLn "Not your piece."
                                                                                                                                                    retry
                                                                                                                                            else do
                                                                                                                                                    let moveList = moves s from -- get all possible moves from selected piece
                                                                                                                                                    if all (/= dest) moveList -- check if destination is valid
                                                                                                                                                        then do putStrLn ("Not a valid destination. Choose from: " ++ show moveList)
                                                                                                                                                                retry
                                                                                                                                                        else do let nextSit = (move s from dest) -- calculate new situation
                                                                                                                                                                gameLoop r nextSit (getNextTree nextSit) depth True -- recurse
                                                                                                                    where
                                                                                                                        retry = gameLoop r s m depth False
                                                                                                                        getNextTree sit = head (filter (testSit (== sit)) list) -- get child that represents new situation (to avoid recalculating)
                                                                                                                        hasLost = lost s
                                                                                            
tryParse :: String -> Bool
tryParse (a:b:c:d:[]) = a >= 'a' && a <= 'h' && b >= '1' && b <= '8' && c >= 'a' && c <= 'h' && d >= '1' && d <= '8'
tryParse _ = False 

parseMove :: String -> (Position, Position)
parseMove inp = (parse (take 2 inp), parse (drop 2 inp))

-- gets minimal number of moves before win
predictLoss' :: HasLost -> Int -> MoveTree (Situation, Bool) -> Int
predictLoss' l n s = if predictLoss l n s
                        then if n > 1
                                then predictLoss' l (n - 2) s -- check if earlier win exists
                                else n
                        else n + 2

-- check for possible win after n moves
predictLoss :: HasLost -> Int -> MoveTree (Situation, Bool) -> Bool
predictLoss l 0 (MoveTree sit list) = snd sit
predictLoss l n (MoveTree sit list) = if odd n
                                            then or (map (predictLoss l (n-1)) list)
                                            else and (map (predictLoss l (n-1)) list)

-- check if situation in MoveTree node fulfils condition
testSit :: (Situation -> Bool) -> MoveTree (Situation, Bool) -> Bool
testSit f (MoveTree (sit, _) _) = f sit