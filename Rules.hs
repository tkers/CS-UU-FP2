module Rules where

import Data.Array
import Data.Char
import Data.List
import Types

chessRules :: Rules
chessRules =    Rules lost moves move
                where
                    lost (Situation (Board b) c) = null (filter (== Piece c King) [b ! (Position (x, y)) | x <- ['a'..'h'], y <- [1..8]]) -- lose if none of the pieces is a king

                    moves (Situation (Board b) _) pos@(Position (x, y)) =   reachablePos (map (filter (validPos)) (map (map (toAbsolute pos)) (moves' typ))) -- convert all relative possible moves to absolute board positions and filter invalid positions
                                                                            where
                                                                                Piece c typ = b ! (Position (x, y))

                                                                                moves' :: PieceType -> [[(Int, Int)]] -- build ascending list of possible relative moves, grouped by direction
                                                                                moves' t = case t of
                                                                                                Pawn -> if c == Black
                                                                                                          then [[(0, -1)], [(-1, -1)], [(1, -1)]]
                                                                                                          else [[(0, 1)], [(-1, 1)], [(1, 1)]]
                                                                                                Rook -> [[(x, 0) | x <- [1..7]]] ++ [[(0, y) | y <- [1..7]]] ++ [(reverse [(x, 0) | x <- [-7..(-1)]])] ++ [(reverse [(0, y) | y <- [-7..(-1)]])]
                                                                                                Bishop -> [[(x, x) | x <- [1..7]]] ++ [[(x, -x) | x <- [1..7]]] ++ [(reverse [(x, x) | x <- [-7..(-1)]])] ++ [(reverse [(x, -x) | x <- [-7..(-1)]])]
                                                                                                Knight -> [[(x, y) | x <- [-2, 2], y <- [-1, 1]] ++ [(x, y) | x <- [-1, 1], y <- [-2, 2]]]
                                                                                                Queen -> (moves' Rook) ++ (moves' Bishop)
                                                                                                King -> [[(-1 , -1)], [(0, -1)], [(1 , -1)], [(-1 , 0)], [(1 , 0)], [(-1 , 1)], [(0 , 1)], [(1 , 1)]]

                                                                                validPos :: Position -> Bool -- check if position is on board
                                                                                validPos (Position (x, y)) =  x >= 'a' && x <= 'h' && y >= 1 && y <= 8

                                                                                reachablePos :: [[Position]] -> [Position] -- check if the piece can reach a position without moving through other pieces
                                                                                reachablePos [] = []
                                                                                reachablePos (x:xs) =   if typ == Knight
                                                                                                            then filter reachableKnight x ++ reachablePos xs -- knight can jump over others
                                                                                                            else if typ == Pawn
                                                                                                                    then reachablePawn (x:xs) -- pawn captures diagonally
                                                                                                                    else reachablePos' x ++ reachablePos xs -- rest

                                                                                reachablePos' :: [Position] -> [Position] -- drop all position behind another piece
                                                                                reachablePos' [] = []
                                                                                reachablePos' (x:xs) =  if d == EmptyPiece
                                                                                                            then x:reachablePos' xs
                                                                                                            else if ((\(Piece col _) -> col) d) /= c
                                                                                                                    then [x] -- capture enemy piece
                                                                                                                    else [] -- hit own piece
                                                                                                        where
                                                                                                            d = b ! x

                                                                                reachablePawn :: [[Position]] -> [Position]
                                                                                reachablePawn (x:y:z:_) =   concat [(if null x -- move forward if position is empty
                                                                                                                        then []
                                                                                                                        else if (b ! head x) == EmptyPiece
                                                                                                                                then x
                                                                                                                                else []),
                                                                                                                    (if null y -- capture diagonally
                                                                                                                        then []
                                                                                                                        else if reachablePawn' (b ! head y)
                                                                                                                                then y
                                                                                                                                else []),
                                                                                                                    (if null z
                                                                                                                        then []
                                                                                                                        else if reachablePawn' (b ! head z)
                                                                                                                                then z
                                                                                                                                else [])]

                                                                                reachablePawn' :: Piece -> Bool -- check if piece is enemy (for diagonal movement)
                                                                                reachablePawn' EmptyPiece = False
                                                                                reachablePawn' (Piece col _) = col /= c

                                                                                reachableKnight :: Position -> Bool -- reachable for knight piece
                                                                                reachableKnight pos = reachableKnight' (b ! pos)

                                                                                reachableKnight' :: Piece -> Bool -- check if place is free or enemy piece
                                                                                reachableKnight' (Piece col _) = col /= c
                                                                                reachableKnight' EmptyPiece = True

                                                                                toAbsolute :: Position -> (Int, Int) -> Position -- from relative to absolute position
                                                                                toAbsolute (Position (x, y)) (xoff, yoff) = Position (chr (ord x + xoff), y + yoff)

                    move (Situation (Board b) curr) from to = Situation newBoard next
                                                                where
                                                                    next = if curr == White -- determine next player
                                                                            then Black
                                                                            else White
                                                                    newBoard = Board (buildNewBoard)
                                                                    buildNewBoard = array (Position ('a', 1), Position ('h', 8)) [(Position (x, y), updatePiece (Position (x, y))) | x <- ['a'..'h'], y <- [1..8]] -- copy old board into new array
                                                                    updatePiece pos = if pos == from
                                                                                        then EmptyPiece -- remove moved piece from old position
                                                                                        else if pos == to -- place moved piece on new position
                                                                                                then b ! from
                                                                                                else b ! pos