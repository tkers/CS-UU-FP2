module Show where

import Data.Array
import Data.Char
import Data.List
import Text.Show.Functions
import Types

instance Show Position where
    show t = showPosition t

instance Show Piece where
    show t = showPiece t

instance Show Board where
    show t = showBoard t

instance Show Situation where
    show t = showSituation t

instance Show PlayerColour where
    show t = showPlayerColour t

showPosition :: Position -> String
showPosition (Position (a, b)) = a : show b

showPiece :: Piece -> String
showPiece EmptyPiece = " "
showPiece (Piece Black t) = case t of
                                Pawn -> "P"
                                Rook -> "R"
                                Bishop -> "B"
                                Knight -> "N"
                                Queen -> "Q"
                                King -> "K"
showPiece (Piece White t) = map toLower (showPiece (Piece Black t))

showBoard :: Board -> String
showBoard t = a ++ b ++ concat ([showLine t i | i <- [1..8]]) ++ b ++ a
              where
                a = intersperse ' ' ("  " ++ ['a'..'h'] ++ "  \n")
                b = "  +" ++ take 17 (repeat '-') ++ "+  \n"

showLine :: Board -> Int -> String
showLine (Board b) i = intersperse ' ' ((show i) ++ "|" ++ [c | r <- ['a'..'h'], c <- show(b!(Position (r, i)))] ++ "|" ++ (show i) ++ "\n")

showSituation :: Situation -> String
showSituation (Situation b c) = "Next player: " ++ (show c) ++ "\n\n" ++ (show b)

showPlayerColour :: PlayerColour -> String
showPlayerColour c = case c of
                        Black -> "black"
                        White -> "white"