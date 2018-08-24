# 3D Projection with Haskell

I recently watched a video from The Coding Train on [3D Rendering with Rotation and Projection](https://www.youtube.com/watch?v=p4Iz0XJY-Qk). It was written in javascript using the p5 library, and it got me wondering if I could recreate it in Haskell.

We will only need two external libraries for this project:

* [Gloss](http://hackage.haskell.org/package/gloss) - For Graphics
* [Matrix](https://hackage.haskell.org/package/matrix-0.3.6.1) - For Matrix Datatype and Operations

You should be able to easily installs these by opening a terminal and using cabal:
```bash
cabal install gloss
cabal install matrix
```
or alternatively with Stack:
```bash
stack install gloss
stack install matrix
```

### _Setting Up Graphics_

We will start out by including two imports:
```haskell
import Graphics.Gloss.Interface.IO.Animate
import Data.Matrix
```

We will be using the "animateIO" function exported from Gloss to display our cube.
```haskell
animateIO :: Display -> Color -> (Float -> IO Picture) -> (Controller -> IO ()) -> IO ()
```

First we'll pass it a Display:
```haskell
main :: IO ()
main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) _ _ _
```

Then we will set the background color to black and pass it a function called "renderCube"

```haskell
main :: IO ()
main = animateIO (InWindow "3D Projection" (800, 800) (240, 360)) black renderCube _
```

We will write the rendeCube function in just a second, but first we need to finish the animateIO function out with a simple function that takes a controller and returns an IO (). We will not be using the controller function.

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

(tuplify is a very sloppy, unsafe function but I could not think of a better way without devoting more time than I wanted to it)

```haskell
pointToMatrix :: (Float, Float, Float) -> Matrix Float
pointToMatrix (x, y, z) = fromList 3 1 [x, y, z]

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
                (50, 50, 50), -- first square top right corner
                (50, (-50), 50),
                ((-50), (-50), 50),
                ((-50), 50, 50),
                (50, 50, (-50)), -- second square top right corner
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

