---
layout: post
title: Pirate Roguelike Dev Log \#1
---

A while ago I began to work on a roguelike name [Vauxhall](https://github.com/HenningTonko/Vauxhall). I never finished it, instead leaving it 
dead on my github with 3-4 premade levels, some sort of inventory system, and one "secret" character. It's a terrible mess. For example, the
way I created a ```[((Int,Int), Char)]``` to represent the tiles on the current screen:
```haskell
  mapWalls :: [String] -> M.Map Coord Char
  mapWalls walls = foldl M.union M.empty $ mapWorld walls 0

  mapLevel :: M.Map Coord Char -> Int -> Int -> String -> M.Map Coord Char
  mapLevel m x y str
    | length str == 1 = M.insert (x, y) (head str) m
    | otherwise = M.insert (x, y) (head str) (mapLevel m (x+1) y (tail str))

  mapWorld :: [String] -> Int -> [M.Map (Int, Int) Char]
  mapWorld strs lvl
    | length strs == 1 = [mapLevel M.empty 0 lvl (head strs)]
    | otherwise = [mapLevel M.empty 0 lvl (head strs)] ++ mapWorld (tail strs) (lvl + 1)
```
Instead of the much simpler:
```haskell
  makeCoordPts :: Int -> Int -> [(Int, Int)]
  makeCoordPts w h = (,) <$> [0..w] <*> [0..h]

  -- Followed by zipping the result with the chars read in from a file
```
Though most of the code is weirdly bloated like above, I really enjoyed how easy it was for me to add features to the game. Aside from Haskell's
powerful type system, the other main reason features were so easy to create was the gameloop I had designed:

```haskell
  gameLoop :: GameState -> IO ()
  gameLoop gs = do
    render gs
    clearOld gs
    a <- case getChar of
           'q' -> Quit
           _ -> Wait
    let gs' = handleInput gs a
    gameLoop gs'

  handleInput :: GameState -> Action -> GameState
  handleInput gs Quit = --quit code
  handleInput gs _ = -- etc
```
This pattern matching in ```handleInput``` kept my code clean and tightly packed to chunks I could easily search for.

I've recently gotten into roguelikes again and found myself itching to write a new, better, and more importantly, complete game. Searching
around for inspiration I came across many people saying they wished their was a pirate themed roguelike. This was perfect! I'm a fan of pirate
games and now I know some people will at least have a small amount of interest in my game.

In the next post I will run through the bare bones code I have so far, and packaging it in a way which will allow myself and others to use it
as a sort of barebones library for creating other games (I plan to use it for 7DRL).

You can find the next post [here](https://henningtonko.github.io/Pirate-Roguelike-2/)