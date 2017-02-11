{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text

-- basic blocks
data Tile
  = Wall
  | Ground
  | Storage
  | Box
  | Blank
  | Player

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
drawTile Box = box & ground
drawTile Blank = blank
drawTile Player = player D & ground

-- Maze Construct
data List a
  = Empty
  | Entry a
          (List a)
  deriving (Eq)

newtype Maze =
  Maze (Coord -> Tile)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

pictureOfBoxes :: Maze -> List Coord -> Picture
pictureOfBoxes mz = combine . mapList drawBox
  where
    drawBox :: Coord -> Picture
    drawBox c =
      if isOnStorage mz c
        then atCoord c $ colored (light brown) (drawTile Box)
        else atCoord c (drawTile Box)

isOnStorage :: Maze -> Coord -> Bool
isOnStorage (Maze mz) c =
  case mz c of
    Storage -> True
    _ -> False

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

pictureOfMaze :: Maze -> Picture
pictureOfMaze (Maze noBox) = putCol (-10)
  where
    putCol :: Integer -> Picture
    putCol x
      | x == 11 = blank
      | otherwise = putBlock (C x (-10)) & putCol (x + 1)
    putBlock :: Coord -> Picture
    putBlock c@(C _ y)
      | y == 11 = blank
      | otherwise = atCoord c (drawTile (noBox c)) & putBlock (adjacentCoord U c)
    putBlock Nowhere = blank

maze :: Coord -> Tile
maze (C x y)
  | x == 0 && y == -1 = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground
maze Nowhere = Blank

noBoxMaze :: Maze -> Maze
noBoxMaze (Maze mz) = Maze mz'
  where
    mz' c =
      case mz c of
        Box -> Ground
        Player -> Ground
        t -> t

initialState :: Maze -> State
initialState (Maze raw) = State initialPostion D initialBoxes
  where
    initialBoxes :: List Coord
    initialBoxes = scan raw upleft
      where
        scan :: (Coord -> Tile) -> Coord -> List Coord
        scan _ (C _ 11) = Empty
        scan maze' c =
          case maze' c of
            Box -> Entry c (scan maze' (next c))
            _ -> scan maze' (next c)
    initialPostion :: Coord
    initialPostion = scan1 raw upleft
      where
        scan1 :: (Coord -> Tile) -> Coord -> Coord
        scan1 _ (C _ 11) = Nowhere
        scan1 maze' c =
          case maze' c of
            Player -> c
            _ -> scan1 maze' (next c)
    next :: Coord -> Coord
    next (C 11 y) = C (-10) (y + 1)
    next (C x y) = C (x + 1) y
    next Nowhere = Nowhere
    upleft :: Coord
    upleft = C (-10) (-10)

-- Movements and Coordination
data Direction
  = R
  | L
  | U
  | D
  deriving Eq

data Coord
  = C Integer
      Integer
  | Nowhere
  deriving (Eq)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromInteger x) (fromInteger y) p
atCoord Nowhere _ = blank

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord D (C x y) = C x (y - 1)
adjacentCoord _ Nowhere = Nowhere

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
  deriving (Eq)

data SSState world
  = StartScreen
  | Running world

winning :: Maze -> List Coord -> Bool
winning m = allList . mapList (isOnStorage m)
  where
    allList :: List Bool -> Bool
    allList Empty = True
    allList (Entry x xs) = x && allList xs

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!") & hint "Press SPACE to start"

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

handleEvent :: Maze -> Event -> State -> State
handleEvent mz _ s@(State _ _ boxList)
  | winning mz boxList = s
handleEvent mz (KeyPress key) s
  | key == "Right" = go mz R s
  | key == "Left" = go mz L s
  | key == "Up" = go mz U s
  | key == "Down" = go mz D s
handleEvent _ _ s = s

go :: Maze -> Direction -> State -> State
go (Maze mz) d s@(State c _ boxList)
  | isPush nextCoord curMaze = State nextCoord d (makeNewList nextCoord d boxList)
  | isMove nextCoord curMaze = State nextCoord d boxList
  | otherwise = s
  where
    nextCoord = adjacentCoord d c
    curMaze = Maze (makeMaze mz boxList)
      where
        makeMaze :: (Coord -> Tile) -> List Coord -> Coord -> Tile
        makeMaze mz' Empty pos = mz' pos
        makeMaze mz' (Entry c' cs) pos
          | pos == c' = Box
          | otherwise = makeMaze mz' cs pos
    isMove :: Coord -> Maze -> Bool
    isMove c' (Maze cur) =
      case cur c' of
        Ground -> True
        Storage -> True
        _ -> False
    isPush :: Coord -> Maze -> Bool
    isPush c' (Maze cur) =
      case (cur c', cur (adjacentCoord d c')) of
        (Box, Ground) -> True
        (Box, Storage) -> True
        _ -> False
    makeNewList :: Coord -> Direction -> List Coord -> List Coord
    makeNewList _ _ Empty = Empty
    makeNewList pos d' (Entry c' cs)
      | pos == c' = Entry (adjacentCoord d' c') cs
      | otherwise = Entry c' (makeNewList pos d' cs)

drawState :: Maze -> State -> Picture
drawState m (State pos dir boxList) =
  winPhase & atCoord pos (player dir) & pictureOfBoxes m boxList & pictureOfMaze m
  where
    winPhase =
      if winning m boxList
        then hint "Press ESC to restart" & scaled 3 3 (text "Winning!") &
             colored (translucent (gray 0.5)) (cube 21)
        else blank

basicInteraction :: Maze -> Interaction State
basicInteraction m =
  Interaction (initialState m) (\_ s -> s) (handleEvent (noBoxMaze m)) (drawState (noBoxMaze m))

main :: IO ()
main =
  let m = Maze maze
  in (runInteractionOf . resetable . withStartScreen . withUndo) (basicInteraction m)

hint :: Text -> Picture
hint t = translated 0 (-5) (text t)

-- Undo
data WithUndo a =
  WithUndo a
           (List a)

withUndo
  :: Eq a
  => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 timer handle draw) = Interaction state0' timer' handle' draw'
  where
    state0' = WithUndo state0 Empty
    timer' t (WithUndo s stack) = WithUndo (timer t s) stack
    handle' (KeyPress key) (WithUndo s stack)
      | key == "U" =
        case stack of
          Entry s' stack' -> WithUndo s' stack'
          Empty -> WithUndo s Empty
    handle' e (WithUndo s stack)
      | s' == s = WithUndo s stack
      | otherwise = WithUndo s' (Entry s stack)
      where
        s' = handle e s
    draw' (WithUndo s _) = draw s
