-- basic blocks of sokoban
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

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

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4 || abs y > 4 = 0
  | abs x == 4 || abs y == 4 = 1
  | x == 2 && y <= 0 = 1
  | x == 3 && y <= 0 = 3
  | x >= -2 && y == 0 = 4
  | otherwise = 2

pictureOfMaze :: Picture
pictureOfMaze = putBlock (-10) (-10)
  where
    putBlock x y
      | x == 11 && y == 10 = blank
      | x == 11 = putBlock (-10) (y + 1)
      | otherwise =
        translated (fromInteger x) (fromInteger y) (drawTile (maze x y)) & putBlock (x + 1) y

main :: IO ()
main = drawingOf pictureOfMaze
