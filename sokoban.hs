{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text
import Data.Maybe

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

-- Self defined list-structure
data List a
  = Empty
  | Entry a
          (List a)
  deriving (Eq)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

{-
elemList
  :: Eq a
  => a -> List a -> Bool
elemList _ Empty = False
elemList x (Entry xx xs)
  | x == xx = True
  | otherwise = elemList x xs

appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Entry x xs) ys = Entry x (appendList xs ys)

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry _ xs) = 1 + listLength xs

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList f (Entry x xs)
  | f x = Entry x (filterList f xs)
  | otherwise = filterList f xs
-}
nth :: List a -> Integer -> Maybe a
nth Empty _ = Nothing
nth (Entry x xs) n
  | n <= 0 = Nothing
  | n == 1 = Just x
  | otherwise = nth xs (n - 1)

-- Maze Construct
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

noBoxMaze :: Maze -> Maze
noBoxMaze (Maze mz) = Maze mz'
  where
    mz' c =
      case mz c of
        Box -> Ground
        Player -> Ground
        t -> t

initialState :: Integer -> State
initialState n = State initialPostion D initialBoxes n
  where
    raw = (\(Maze mz) -> mz) (nthMaze mazes n)
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
  deriving (Eq)

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
        Integer
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

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (State _ _ boxList level)
  | winning mz boxList && key == " " && level < 8 = initialState (level + 1)
  | winning mz boxList && key == " " && level == 8 = initialState 1
  where
    mz = noBoxMaze (nthMaze mazes level)
handleEvent _ s@(State _ _ boxList level)
  | winning mz boxList = s
  where
    mz = noBoxMaze (nthMaze mazes level)
handleEvent (KeyPress key) s@(State _ _ _ level)
  | key == "Right" = go mz R s
  | key == "Left" = go mz L s
  | key == "Up" = go mz U s
  | key == "Down" = go mz D s
  where
    mz = noBoxMaze (nthMaze mazes level)
handleEvent _ s = s

go :: Maze -> Direction -> State -> State
go (Maze mz) d s@(State c _ boxList level)
  | isPush nextCoord curMaze = State nextCoord d (makeNewList nextCoord d boxList) level
  | isMove nextCoord curMaze = State nextCoord d boxList level
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

drawState :: State -> Picture
drawState (State pos dir boxList level) =
  winPhase & atCoord pos (player dir) & pictureOfBoxes m boxList & pictureOfMaze m
  where
    m = noBoxMaze (nthMaze mazes level)
    winPhase =
      if winning m boxList
        then hint "Press SPACE to continue" &
             translated 0 (-1) (hint "or Press ESC to restart") &
             scaled 3 3 (text "Winning!") &
             colored (translucent (gray 0.5)) (cube 21)
        else blank

basicInteraction :: Integer -> Interaction State
basicInteraction m = Interaction (initialState m) (\_ s -> s) handleEvent drawState

main :: IO ()
main = (runInteractionOf . resetable . withStartScreen . withUndo) (basicInteraction 1)

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

-- Mazes
newtype Maze =
  Maze (Coord -> Tile)

mazes :: List Maze
mazes =
  Entry (Maze maze9) $
  Entry (Maze maze8) $
  Entry (Maze maze7) $
  Entry (Maze maze6) $
  Entry (Maze maze5) $ Entry (Maze maze4) $ Entry (Maze maze3) $ Entry (Maze maze1) Empty

nthMaze :: List Maze -> Integer -> Maze
nthMaze mazeList n = fromMaybe (Maze maze1) (nth mazeList n)

{-
extraMazes :: List Maze
extraMazes = Entry (Maze maze4') $ Entry (Maze maze4'') $ Entry (Maze maze9') mazes
-}
maze1 :: Coord -> Tile
maze1 (C x y)
  | x == 0 && y == 1 = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground
maze1 Nowhere = Blank

maze3 :: Coord -> Tile
maze3 (C (-5) (-5)) = Wall
maze3 (C (-5) (-4)) = Wall
maze3 (C (-5) (-3)) = Wall
maze3 (C (-5) (-2)) = Wall
maze3 (C (-5) (-1)) = Wall
maze3 (C (-5) 0) = Wall
maze3 (C (-5) 1) = Wall
maze3 (C (-5) 2) = Wall
maze3 (C (-5) 3) = Wall
maze3 (C (-5) 4) = Wall
maze3 (C (-4) (-5)) = Wall
maze3 (C (-4) (-4)) = Ground
maze3 (C (-4) (-3)) = Ground
maze3 (C (-4) (-2)) = Ground
maze3 (C (-4) (-1)) = Ground
maze3 (C (-4) 0) = Ground
maze3 (C (-4) 1) = Ground
maze3 (C (-4) 2) = Ground
maze3 (C (-4) 3) = Player
maze3 (C (-4) 4) = Wall
maze3 (C (-3) (-5)) = Wall
maze3 (C (-3) (-4)) = Ground
maze3 (C (-3) (-3)) = Wall
maze3 (C (-3) (-2)) = Wall
maze3 (C (-3) (-1)) = Wall
maze3 (C (-3) 0) = Wall
maze3 (C (-3) 1) = Ground
maze3 (C (-3) 2) = Wall
maze3 (C (-3) 3) = Ground
maze3 (C (-3) 4) = Wall
maze3 (C (-3) 5) = Wall
maze3 (C (-2) (-5)) = Wall
maze3 (C (-2) (-4)) = Box
maze3 (C (-2) (-3)) = Ground
maze3 (C (-2) (-2)) = Ground
maze3 (C (-2) (-1)) = Ground
maze3 (C (-2) 0) = Wall
maze3 (C (-2) 1) = Ground
maze3 (C (-2) 2) = Box
maze3 (C (-2) 3) = Box
maze3 (C (-2) 4) = Ground
maze3 (C (-2) 5) = Wall
maze3 (C (-1) (-6)) = Wall
maze3 (C (-1) (-5)) = Wall
maze3 (C (-1) (-4)) = Ground
maze3 (C (-1) (-3)) = Ground
maze3 (C (-1) (-2)) = Ground
maze3 (C (-1) (-1)) = Ground
maze3 (C (-1) 0) = Wall
maze3 (C (-1) 1) = Ground
maze3 (C (-1) 2) = Ground
maze3 (C (-1) 3) = Box
maze3 (C (-1) 4) = Ground
maze3 (C (-1) 5) = Wall
maze3 (C (-1) 6) = Wall
maze3 (C 0 (-6)) = Wall
maze3 (C 0 (-5)) = Ground
maze3 (C 0 (-4)) = Ground
maze3 (C 0 (-3)) = Ground
maze3 (C 0 (-2)) = Ground
maze3 (C 0 (-1)) = Ground
maze3 (C 0 0) = Wall
maze3 (C 0 1) = Wall
maze3 (C 0 2) = Wall
maze3 (C 0 3) = Wall
maze3 (C 0 4) = Ground
maze3 (C 0 5) = Ground
maze3 (C 0 6) = Wall
maze3 (C 1 (-6)) = Wall
maze3 (C 1 (-5)) = Ground
maze3 (C 1 (-4)) = Ground
maze3 (C 1 (-3)) = Ground
maze3 (C 1 (-2)) = Ground
maze3 (C 1 (-1)) = Ground
maze3 (C 1 0) = Wall
maze3 (C 1 1) = Storage
maze3 (C 1 2) = Storage
maze3 (C 1 3) = Storage
maze3 (C 1 4) = Ground
maze3 (C 1 5) = Ground
maze3 (C 1 6) = Wall
maze3 (C 2 (-6)) = Wall
maze3 (C 2 (-5)) = Wall
maze3 (C 2 (-4)) = Ground
maze3 (C 2 (-3)) = Ground
maze3 (C 2 (-2)) = Ground
maze3 (C 2 (-1)) = Ground
maze3 (C 2 0) = Wall
maze3 (C 2 1) = Wall
maze3 (C 2 2) = Wall
maze3 (C 2 3) = Wall
maze3 (C 2 4) = Wall
maze3 (C 2 5) = Wall
maze3 (C 2 6) = Wall
maze3 (C 3 (-5)) = Wall
maze3 (C 3 (-4)) = Ground
maze3 (C 3 (-3)) = Ground
maze3 (C 3 (-2)) = Storage
maze3 (C 3 (-1)) = Ground
maze3 (C 3 0) = Wall
maze3 (C 4 (-5)) = Wall
maze3 (C 4 (-4)) = Wall
maze3 (C 4 (-3)) = Wall
maze3 (C 4 (-2)) = Wall
maze3 (C 4 (-1)) = Wall
maze3 (C 4 0) = Wall
maze3 _ = Blank

maze4 :: Coord -> Tile
maze4 (C x y)
  | x == 1 && y == (-3) = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y < 0 = Wall
  | x >= -1 && y == 1 && x <= 2 = Wall
  | x == -3 && y == 1 = Wall
  | x == 0 && y == 3 = Wall
  | x == 0 && y == 0 = Wall
  | x == 3 && y == -3 = Storage
  | x == 1 && y == 2 = Storage
  | x == -3 && y == 2 = Storage
  | x == 1 && y == -1 = Storage
  | x == -2 && y == 1 = Box
  | x == 2 && y == 2 = Box
  | x <= 1 && y == -2 && x >= 0 = Box
  | otherwise = Ground
maze4 Nowhere = Blank

maze5 :: Coord -> Tile
maze5 (C x y)
  | x == 0 && y == 1 = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 1 && y < 0 = Wall
  | x == -3 && y == -2 = Wall
  | x <= 1 && x > -2 && y == 0 = Wall
  | x > -3 && x < 3 && y == 2 = Wall
  | x == 3 && y > 1 = Storage
  | y == -2 && x < 0 = Box
  | y == -2 && x == 2 = Box
  | y == 0 && x == 3 = Box
  | y == -1 && x > 1 && x < 4 = Storage
  | otherwise = Ground
maze5 Nowhere = Blank

maze6 :: Coord -> Tile
maze6 (C x y)
  | x == (-2) && y == 4 = Player
  | abs x > 3 || abs y > 5 = Blank
  | abs x == 3 || (abs y == 5 && abs x < 4) = Wall
  | x == 0 && abs y < 4 = Storage
  | x == -1 && (y == 0 || abs y == 2) = Box
  | x == 1 && (abs y == 1 || abs y == 3) = Box
  | x == (-2) && y == 1 = Wall
  | otherwise = Ground
maze6 Nowhere = Blank

maze7 :: Coord -> Tile
maze7 (C x y)
  | x == (-3) && y == 3 = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x /= 2 && y == 2 = Wall
  | x /= -2 && y == -1 = Wall
  | x == 3 && y == -3 = Storage
  | x == 2 && y == 2 = Box
  | otherwise = Ground
maze7 Nowhere = Blank

maze8 :: Coord -> Tile
maze8 (C x y)
  | x == 0 && y == 0 = Player
  | abs x > 10 || abs y > 10 = Blank
  | x == 0 && y == 0 = Ground
  | abs x == 9 && abs y == 9 = Wall
  | abs x == 10 || abs y == 10 = Wall
  | x == y = Storage
  | abs x == abs y = Box
  | x < 0 && x > (-9) && y == 0 = Box
  | x > 0 && x < 9 && y == 0 = Storage
  | otherwise = Ground
maze8 Nowhere = Blank

maze9 :: Coord -> Tile
maze9 (C x y)
  | x == 1 && y == 1 = Player
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 || x == -3 = Wall
  | x == -2 && (y == 3 || y == 0) = Wall
  | x == -1 && y == -1 = Wall
  | x == -0 && y == 1 = Wall
  | x == 3 && y == 0 = Wall
  | x < 0 && (y == 2 || y == -3) = Storage
  | x == -1 && y == 1 = Storage
  | x == 0 && (y == 2 || y == 0 || y == -1) = Box
  | x == 1 && y == -2 = Box
  | x == 2 && y == -3 = Box
  | otherwise = Ground
maze9 Nowhere = Blank
{-
maze4'' :: Coord -> Tile
maze4'' (C 1 (-3)) = Box
maze4'' c = maze4 c

maze4' :: Coord -> Tile
maze4' (C 0 1) = Blank
maze4' c = maze4 c

maze9' :: Coord -> Tile
maze9' (C 3 0) = Box
maze9' (C 4 0) = Box
maze9' c = maze9 c
-}
