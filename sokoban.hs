-- basic blocks of sokoban
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank

cube :: Double -> Picture
cube a = solidRectangle a a

wall :: Picture
wall = colored (gray 0.5) (cube 1)

ground :: Picture
ground = colored (dark yellow) (cube 1)

storage :: Picture
storage = solidCircle 0.2 & ground

box :: Picture
box = colored (dark brown) (cube 0.9) & ground

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

pictureOfMaze :: Picture
pictureOfMaze = putCol (-10)
  where
    putCol :: Integer -> Picture
    putCol x
      | x == 11 = blank
      | otherwise = putBlock (C x (-10)) & putCol (x + 1)
    putBlock :: Coord -> Picture
    putBlock c@(C x y)
      | y == 11 = blank
      | otherwise = atCoord c (drawTile (maze c)) & putBlock (adjacentCoord U c)

data Direction
  = R
  | L
  | U
  | D

data Coord =
  C Integer
    Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromInteger x) (fromInteger y)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord D (C x y) = C x (y - 1)

main :: IO ()
main = interactionOf initialCoord handleTime handleEvent drawState

handleTime :: Double -> Coord -> Coord
handleTime _ c = c -- TODO

handleEvent :: Event -> Coord -> Coord
handleEvent _ _ = initialCoord -- TODO

drawState :: Coord -> Picture
drawState _ = blank -- TODO
