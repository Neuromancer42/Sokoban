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
storage = colored white (solidCircle 0.2) & ground

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
    putBlock c@(C _ y)
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

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromInteger x) (fromInteger y)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord D (C x y) = C x (y - 1)

player :: Direction -> Picture
player R =
  translated 0 0.3 cranium & path [(0, 0), (0.3, 0.05)] & path [(0, 0), (0.3, -0.05)] &
  path [(0, -0.2), (0, 0.1)] &
  path [(0, -0.2), (0.1, -0.5)] &
  path [(0, -0.2), (-0.1, -0.5)]
  where
    cranium = circle 0.18 & sector (7 / 6 * pi) (1 / 6 * pi) 0.18
player L = scaled (-1) 1 (player R)
player U =
  translated 0 0.3 cranium & path [(0, 0), (0.3, 0.05)] & path [(0, 0), (-0.3, 0.05)] &
  path [(0, -0.2), (0, 0.1)] &
  path [(0, -0.2), (0.1, -0.5)] &
  path [(0, -0.2), (-0.1, -0.5)]
  where
    cranium = solidCircle 0.18
player D =
  translated 0 0.3 cranium & path [(0, 0), (0.3, -0.05)] & path [(0, 0), (-0.3, -0.05)] &
  path [(0, -0.2), (0, 0.1)] &
  path [(0, -0.2), (0.1, -0.5)] &
  path [(0, -0.2), (-0.1, -0.5)]
  where
    cranium = circle 0.18

initialState :: State
initialState = State (C 0 (-1)) R

data State =
  State Coord
        Direction

data SSState world
  = StartScreen
  | Running world

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data Interaction world =
  Interaction world
              (Double -> world -> world)
              (Event -> world -> world)
              (world -> Picture)

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 timer handle draw) = Interaction state0 timer handle' draw
  where
    handle' (KeyPress key) _
      | key == "Esc" = state0
    handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 timer handle draw) = Interaction state0' timer' handle' draw'
  where
    state0' = StartScreen
    timer' _ StartScreen = StartScreen
    timer' t (Running s) = Running (timer t s)
    handle' (KeyPress key) StartScreen
      | key == " " = Running state0
    handle' _ StartScreen = StartScreen
    handle' k (Running s) = Running (handle k s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s

runInteractionOf :: Interaction s -> IO ()
runInteractionOf (Interaction state0 timer handle draw) = interactionOf state0 timer handle draw

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (State c _)
  | key == "Right" = State (tryStep c R) R
  | key == "Left" = State (tryStep c L) L
  | key == "Up" = State (tryStep c U) U
  | key == "Down" = State (tryStep c D) D
handleEvent _ s = s

tryStep :: Coord -> Direction -> Coord
tryStep cur dir
  | isOk (maze next) = next
  | otherwise = cur
  where
    next :: Coord
    next = adjacentCoord dir cur
    isOk :: Tile -> Bool
    isOk Ground = True
    isOk Storage = True
    isOk _ = False

drawState :: State -> Picture
drawState (State pos dir) = atCoord pos (player dir) & pictureOfMaze

basicInteraction :: Interaction State
basicInteraction = Interaction initialState (\_ s -> s) handleEvent drawState

main :: IO ()
main = (runInteractionOf . resetable . withStartScreen) basicInteraction
