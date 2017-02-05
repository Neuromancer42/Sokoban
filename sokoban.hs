-- basic blocks of sokoban
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank

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
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Integer -> Integer -> Tile
maze x y
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
      | otherwise = putBlock x (-10) & putCol (x + 1)
    putBlock :: Integer -> Integer -> Picture
    putBlock x y
      | y == 11 = blank
      | otherwise =
        translated (fromInteger x) (fromInteger y) (drawTile (maze x y)) & putBlock x  (y + 1)

main :: IO ()
main = drawingOf pictureOfMaze
