{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

-- basic blocks
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
box = colored (dark brown) (cube 0.9)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

-- Maze Construct
data List a
  = Empty
  | Entry a
          (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes = combine . mapList drawBox
  where
    drawBox :: Coord -> Picture
    drawBox c =
      if isOnStorage c
        then atCoord c $ colored (light brown) (drawTile Box)
        else atCoord c (drawTile Box)

isOnStorage :: Coord -> Bool
isOnStorage c =
  case noBoxMaze c of
    Storage -> True
    _ -> False

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

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
      | otherwise = atCoord c (drawTile (noBoxMaze c)) & putBlock (adjacentCoord U c)

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c =
  case maze c of
    Box -> Ground
    t -> t

initialState :: State
initialState = State (C 0 (-1)) D (initialBoxes maze)

initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes maze' = scan maze' upleft
  where
    upleft :: Coord
    upleft = C (-10) (-10)
    scan :: (Coord -> Tile) -> Coord -> List Coord
    scan _ (C _ 11) = Empty
    scan maze'' c =
      case maze'' c of
        Box -> Entry c (scan maze'' (next c))
        _ -> scan maze'' (next c)
    next :: Coord -> Coord
    next (C 11 y) = C (-10) (y + 1)
    next (C x y) = C (x + 1) y

-- Movements and Coordination
data Direction
  = R
  | L
  | U
  | D

data Coord =
  C Integer
    Integer
  deriving (Eq)

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

-- State and Interaction
data State =
  State Coord
        Direction
        (List Coord)

data SSState world
  = StartScreen
  | Running world

winning :: List Coord -> Bool
winning = allList . mapList isOnStorage
  where
    allList :: List Bool -> Bool
    allList Empty = True
    allList (Entry x xs) = x && allList xs

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
handleEvent _ s@(State _ _ boxList) | winning boxList = s
handleEvent (KeyPress key) s
  | key == "Right" = go R s
  | key == "Left" = go L s
  | key == "Up" = go U s
  | key == "Down" = go D s
handleEvent _ s = s

go :: Direction -> State -> State
go d s@(State c _ boxList)
  | isPush nextCoord curMaze = State nextCoord d (makeNewList nextCoord d boxList)
  | isMove nextCoord curMaze = State nextCoord d boxList
  | otherwise = s
  where
    nextCoord = adjacentCoord d c
    curMaze = makeMaze noBoxMaze boxList
      where
        makeMaze :: (Coord -> Tile) -> List Coord -> Coord -> Tile
        makeMaze mz Empty pos = mz pos
        makeMaze mz (Entry c' cs) pos
          | pos == c' = Box
          | otherwise = makeMaze mz cs pos
    isMove :: Coord -> (Coord -> Tile) -> Bool
    isMove c' mz =
      case mz c' of
        Ground -> True
        Storage -> True
        _ -> False
    isPush :: Coord -> (Coord -> Tile) -> Bool
    isPush c' mz =
      case (mz c', mz (adjacentCoord d c')) of
        (Box, Ground) -> True
        (Box, Storage) -> True
        _ -> False
    makeNewList :: Coord -> Direction -> List Coord -> List Coord
    makeNewList _ _ Empty = Empty
    makeNewList pos d' (Entry c' cs)
      | pos == c' = Entry (adjacentCoord d' c') cs
      | otherwise = Entry c' (makeNewList pos d' cs)

drawState :: State -> Picture
drawState (State pos dir boxList) =
  winPhase & atCoord pos (player dir) & pictureOfBoxes boxList & pictureOfMaze
  where
    winPhase =
      if winning boxList
        then scaled 3 3 (text "Winning!") & colored (translucent (gray 0.5)) (cube 21)
        else blank

basicInteraction :: Interaction State
basicInteraction = Interaction initialState (\_ s -> s) handleEvent drawState

main :: IO ()
main = (runInteractionOf . resetable . withStartScreen) basicInteraction
-- main = drawingOf ((\(State c d boxList) -> (atCoord c (player d)) & pictureOfBoxes boxList) initialState)
