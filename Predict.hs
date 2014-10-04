module Predict where

import Types
import Data.Array

-- build tree of all possible situations arising from valid moves
predict :: Rules -> Situation -> MoveTree (Situation, Bool)
predict r@(Rules lost moves move) s@(Situation (Board a) curr) = MoveTree (s, lost s) possibles
                                                                        where
                                                                            possibles = concat [getSits (Position (x, y)) | x <- ['a'..'h'], y <- [1..8], isSameColour (Position (x,y))]
                                                                            isSameColour pos = isSameColour' (a ! pos)
                                                                            isSameColour' (Piece c _) = c == curr
                                                                            isSameColour' EmptyPiece = False
                                                                            getSits pos = map (predict r) (map (move s pos) (moves s pos))
                                                                            tupleLoss sit = (sit, lost sit)