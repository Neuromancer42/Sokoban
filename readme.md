# Sokoban Game

This is an interesting game. You can control the guy on the map using your direction keys ( ↑ ↓ ← → ). Help him **push** all the boxes onto the storage sites.

## How to run the this game
1. If you have **cabal** on your computer, use the following command to start the program:
``
cabal exec -- runghc sokoban.hs
``


2. If you do not have such tool, the following site is also available. \\
[CodeWorld][1]

[1]: https://code.world/run.html?mode=haskell&dhash=D_6FfdGCInzAWXIHxVOL-1Q



## TODO
1. add more maps
2. implement File IO, convert files into maps (actually, the map here is a function Coord → Tile)
