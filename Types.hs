module Types where

import Data.Array

data PieceType = Pawn | Rook | Bishop | Knight | Queen | King deriving (Eq)
data PlayerColour = Black | White deriving (Eq)
data Piece = Piece PlayerColour PieceType | EmptyPiece deriving (Eq)

data Position = Position (Char, Int) deriving (Ix, Ord, Eq)
data Board = Board (Array Position Piece) deriving (Eq)
data Situation = Situation Board PlayerColour deriving (Eq)

data Rules = Rules HasLost MovesFrom PerformMove
type HasLost = Situation -> Bool
type MovesFrom = Situation -> Position -> [Position]
type PerformMove = Situation -> Position -> Position -> Situation

data MoveTree a = MoveTree a [MoveTree a]