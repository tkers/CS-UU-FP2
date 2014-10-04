module Parse where

import Data.Array
import Data.Char
import Data.List
import Rules
import Show
import Types

class Show a => Parse a where
    parse :: String -> a

instance Parse Position where
    parse (x:y:[]) = Position (x, digitToInt y)

instance Parse Piece where
    parse (x:[]) = case y of
                        'P' -> Piece z Pawn
                        'R' -> Piece z Rook
                        'B' -> Piece z Bishop
                        'N' -> Piece z Knight
                        'Q' -> Piece z Queen
                        'K' -> Piece z King
                        ' ' -> EmptyPiece
                        _   -> EmptyPiece
                    where   y = toUpper x
                            z = if y == x
                                    then Black
                                    else White

instance Parse Board where
    parse s =   Board (array (Position ('a', 1), Position ('h', 8)) [(Position (a, b), parse [(getPiece a b l)]) | a <- ['a'..'h'], b <- [1..8]])
                where
                    l = map (dropTake 4 15) (dropTake 2 8 (lines s)) -- extract board contents
                    dropTake x y = (take y) . (drop x)
                    getPiece a b l = headDrop ((ord a - ord 'a') * 2) (headDrop (b - 1) l)
                    headDrop n = head . (drop n)

instance Parse PlayerColour where
    parse s =   case s of
                    "white" -> White
                    "black" -> Black

instance Parse Situation where
    parse s =   Situation (parse b) (parse a)
                where
                    l = lines s
                    a = drop 13 (head l)
                    b = concat (intersperse "\n" (drop 2 l))