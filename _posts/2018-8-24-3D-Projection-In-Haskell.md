---
layout: post
title: 3D projection in Haskell
---

# 3D Projection with Haskell

I recently watched a video from The Coding Train on [3D Rendering with Rotation and Projection](https://www.youtube.com/watch?v=p4Iz0XJY-Qk). It was written in javascript using the p5 library, and showed how to draw 3D graphics using a 2D graphics library. It got me wondering if I could recreate it in Haskell.

We will only need two external libraries for this project:

* [Gloss](http://hackage.haskell.org/package/gloss) - For Graphics
* [Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1) - For Matrix Datatype and Operations

You should be able to easily install these by opening a terminal and using cabal:
```bash
cabal install gloss
cabal install matrix
```
or alternatively with Stack (which I don't use):
```bash
stack install gloss
stack install matrix
```

### _Setting Up The Graphics_

We will start out by including two imports:
```haskell
import Graphics.Gloss.Interface.IO.Animate
import Data.Matrix
```

We will be using the "animateIO" function exported from Gloss to display our cube.
```haskell
animateIO :: Display -> Color -> (Float -> IO Picture) -> (Controller -> IO ()) -> IO ()
```

First we'll pass it a Display with the title "3D Projection", the size of 800x800, and the screen position of (240, 360) (roughly in the middle of my 1080p monitor):
```haskell
main :: IO ()
main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) _ _ _
```

Then we will set the background color to black and pass it a function called "renderCube"

```haskell
main :: IO ()
main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) black renderCube _
```

We will write the renderCube function in just a second, but first we need to finish the animateIO function out with a simple function that takes a Controller and returns an IO (). We will not be using the controller function so a simple lambda statement will suffice.

```haskell
main :: IO ()
main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) black renderCube (\c -> return ())
```

### _Representing a 3D object_

We will be representing each point of our cube as a tuple of the type:
```haskell
(Float, Float, Float) -- (x, y, z)
[(Float, Float, Float)] -- list of points of the cube
```

We will need to be able to convert this tuple to and from a Matrix in order to do the correct Rotation and Projection Matrix math needed to draw the 3D shape in 2D. For this we will define three simple functions:

(tuplify is a very sloppy, unsafe function but I could not think of a better way without devoting more time than I wanted to, to it)

```haskell
-- make a 3x1 matrix from a tuple
pointToMatrix :: (Float, Float, Float) -> Matrix Float
pointToMatrix (x, y, z) = fromList 3 1 [x, y, z]

-- make a tuple from a 3x1 matrix
matrixToPoint :: Matrix Float -> (Float, Float)
matrixToPoint m = tuplify $ toList m

tuplify :: [a] -> (a, a)
tuplify [x, y] = (x, y)
```

fromList and toList have the following types and are exported by Data.Matrix
```haskell
fromList :: Int -> Int -> [a] -> Matrix a
toList :: Matrix a -> [a]
```

We will now define the points of our cube:
```haskell
cubePoints3D :: [(Float, Float, Float)]
cubePoints3D = [
                (50, 50, 50), -- foreground square top right corner
                (50, (-50), 50),
                ((-50), (-50), 50),
                ((-50), 50, 50),
                (50, 50, (-50)), -- background square top right corner
                (50, (-50), (-50)),
                ((-50), (-50), (-50)),
                ((-50), 50, (-50))
              ]
```

Now we can start to define the various matrices we will use for rotations and transformations.

### Rotation and Transformation Matrices

We will first define a matrix for projecting a 3D point to 2D.
```haskell
projection :: Matrix Float
projection = fromList 2 3 [
                          1, 0, 0,
                          0, 1, 0
                          ]
```

Next we define matrices for rotating around the three axis:
```haskell
rotX :: Float -> Matrix Float
rotX θ = fromList 3 3 [
                        1, 0, 0,
                        0, cos θ, negate (sin θ),
                        0, sin θ, cos θ
                      ]

rotY :: Float -> Matrix Float
rotY θ = fromList 3 3 [
                        cos θ, 0, negate (sin θ),
                        0, 1, 1,
                        sin θ, 0, cos θ
                      ]

rotZ :: Float -> Matrix Float
rotZ θ = fromList 3 3 [
                        cos θ, negate (sin θ), 0,
                        sin θ, cos θ, 0,
                        0, 0, 1
                      ]
```

Data.Matrix includes an instance of the Num typeclass for Matrix a, which means we can simply use ( * ) to multiply two matrices together. It is important that we pass ( * ) the smaller of the two matrices first, followed by the other (larger) matrix. This avoids crashing, due to how matrix multiplication works.

First we want to apply the rotation matrices to the cube points. In this case I am applying a Y and X axis rotation.
```haskell
cubePointsRotated :: Float -> [Matrix Float]
cubePointsRotated t = map ((*) (rotY yRotationValue)) xRotated -- apply yRotation
  where
    cubePoints = map pointToMatrix cubePoints3D -- convert points to a matrix
    xRotated = map ((*) (rotX t)) cubePoints -- apply xRotation
```
The argument "t" is passed in from the renderCube function. Its essentially a Float representing the time since the program was started and is used in this case as an ever changing angle for our rotX function.

Next we apply the projection matrix to the list of rotated point matrices
```haskell
cubePoints2D :: Float -> [Matrix Float]
cubePoints2D t = map ((*) projection) (cubePointsRotated t)
```

Finally we convert the list of 3x1 matrices to a list of coordinate points with rotatedPointsFromMats
```haskell
rotatedPointsFromMats :: Float -> [(Float, Float)]
rotatedPointsFromMats t = map matrixToPoint (cubePoints2D t)
```

### Rendering
Now that we have the points, rotations, and projections done, its pretty simple to draw the cube. We use Pictures to construct a single Picture from a list of Pictures

```haskell
renderCube :: Float -> IO Picture
renderCube t = return $ Pictures [makeSquare 1 firstSqr, makeSquare 2 secondSqr, l]
  where
    points = rotatedPointsFromMats t -- list of points of our cube
    firstSqr = take 4 points -- points for the square in the foreground
    secondSqr = drop 4 points -- points for the square in the background
    l = makeLines points -- lines from corner of square in foreground to background square
```

We will be using line loops for drawing the squares. A line loop takes a list of coordinate points and draws a line through them and then closes the loop. We will draw one square in red, the other in blue to make it a little easier to keep track of the different ends of the square.
```haskell
makeSquare :: Int -> [(Float, Float)] -> Picture
makeSquare n pts = if n == 1 then color blue (lineLoop pts) else color red (lineLoop pts)
```

Connect each corner of a square to the same corner on the other square.
```haskell
makeLines :: [(Float, Float)] -> Picture
makeLines pts = Pictures [color white $ line a, color white $ line b, color white $ line c, color white $ line d]
  where
    a = [pts !! 0, pts !! 4]
    b = [pts !! 1, pts !! 5]
    c = [pts !! 2, pts !! 6]
    d = [pts !! 3, pts !! 7]
```
Done!

Now just compile and run it:
```bash
ghc -o Cube Main.hs
./Cube
```
![](https://github.com/HenningTonko/HenningTonko.github.io/blob/master/images/3Dcube.gif)
* [Code on Github](https://github.com/HenningTonko/3DProjection)

Full Code:
```haskell
module Main where
  import Graphics.Gloss.Interface.IO.Animate
  import Data.Matrix

  yRotationValue :: Float
  yRotationValue = 0.5

  rotX :: Float -> Matrix Float
  rotX θ = fromList 3 3 [
                          1, 0, 0,
                          0, cos θ, negate (sin θ),
                          0, sin θ, cos θ
                        ]

  rotY :: Float -> Matrix Float
  rotY θ = fromList 3 3 [
                          cos θ, 0, negate (sin θ),
                          0, 1, 1,
                          sin θ, 0, cos θ
                        ]

  rotZ :: Float -> Matrix Float
  rotZ θ = fromList 3 3 [
                          cos θ, negate (sin θ), 0,
                          sin θ, cos θ, 0,
                          0, 0, 1
                        ]

  projection :: Matrix Float
  projection = fromList 2 3 [
                              1, 0, 0,
                              0, 1, 0
                            ]

  tuplify :: [a] -> (a, a)
  tuplify [x, y] = (x, y)

  pointToMatrix :: (Float, Float, Float) -> Matrix Float
  pointToMatrix (x, y, z) = fromList 3 1 [x, y, z]

  matrixToPoint :: Matrix Float -> (Float, Float)
  matrixToPoint m = tuplify $ toList m

  cubePoints3D :: [(Float, Float, Float)]
  cubePoints3D = [
                  (50, 50, 50), -- first square top right corner
                  (50, (-50), 50),
                  ((-50), (-50), 50),
                  ((-50), 50, 50),
                  (50, 50, (-50)), -- second square top right corner
                  (50, (-50), (-50)),
                  ((-50), (-50), (-50)),
                  ((-50), 50, (-50))
                ]

  pyramidPoints3D :: [(Float, Float, Float)]
  pyramidPoints3D = [
                      (200, 0, 0),
                      (300, 0, 0),
                      (200, 0, 100),
                      (300, 0, 100),
                      (250, 200, 50)
                    ]

  -- PYRAMID

  pyramidPointsRotated :: Float -> [Matrix Float]
  pyramidPointsRotated t = map ((*) (rotY yRotationValue)) xRotated
    where
      pyramidPoints = map pointToMatrix cubePoints3D
      xRotated = map ((*) (rotX t)) pyramidPoints

  pyramidPoints2D :: Float -> [Matrix Float]
  pyramidPoints2D t = map ((*) projection) (pyramidPointsRotated t)

  rotatedPyramidPointsFromMats :: Float -> [(Float, Float)]
  rotatedPyramidPointsFromMats t = map matrixToPoint (pyramidPoints2D t)

  -- CUBE

  cubePointsRotated :: Float -> [Matrix Float]
  cubePointsRotated t = map ((*) (rotY yRotationValue)) xRotated
    where
      cubePoints = map pointToMatrix cubePoints3D
      xRotated = map ((*) (rotX t)) cubePoints

  cubePoints2D :: Float -> [Matrix Float]
  cubePoints2D t = map ((*) projection) (cubePointsRotated t)

  rotatedPointsFromMats :: Float -> [(Float, Float)]
  rotatedPointsFromMats t = map matrixToPoint (cubePoints2D t)

  -- render functions

  makeSquare :: Int -> [(Float, Float)] -> Picture
  makeSquare n pts = if n == 1 then color blue (lineLoop pts) else color red (lineLoop pts)

  makeLines :: [(Float, Float)] -> Picture
  makeLines pts = Pictures [color white $ line a, color white $ line b, color white $ line c, color white $ line d]
    where
      a = [pts !! 0, pts !! 4]
      b = [pts !! 1, pts !! 5]
      c = [pts !! 2, pts !! 6]
      d = [pts !! 3, pts !! 7]

  connectToTip :: [(Float, Float)] -> Picture
  connectToTip coords = Pictures [color white $ line a, color white $ line b, color white $ line c, color white $ line d]
    where
      a = [coords !! 0, last coords]
      b = [coords !! 1, last coords]
      c = [coords !! 2, last coords]
      d = [coords !! 3, last coords]

  renderCube :: Float -> IO Picture
  renderCube t = return $ Pictures [makeSquare 1 firstSqr, makeSquare 2 secondSqr, makeLines points] -- ++ [makeSquare 1 pyramidBase, connectToTip (rotatedPyramidPointsFromMats yRotationValue t)]
    where
      points = rotatedPointsFromMats t
      firstSqr = take 4 points
      secondSqr = drop 4 points
      pyramidBase = take 4 $ rotatedPyramidPointsFromMats t

  main :: IO ()
  main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) black renderCube (\c -> return ())
```