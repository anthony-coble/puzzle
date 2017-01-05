{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
 
import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as CUDA
import Prelude as P hiding ((==), (&&), (/=), rem, filter)

type Tile = (Edge, Edge, Edge, Edge)
type Edge = (Int, Int)

front = 5 :: Int
back = 6 :: Int

goat = 1 :: Int
coyote = 2 :: Int
cougar = 3 :: Int
ram = 4 :: Int

items = ( ( ( goat, front)
          , ( ram, back)
          , ( cougar, front)
          , ( coyote, back)
          )
        , ( ( cougar, back)
          , ( goat, front)
          , ( ram, back)
          , ( ram, front)
          )
        , ( ( cougar, front)
          , ( coyote, front)
          , ( ram, back)
          , ( goat, back)
          )
        , ( ( coyote, back)
          , ( goat, back)
          , ( ram, front)
          , ( cougar, front)
          )
        , ( ( ram, front)
          , ( goat, front)
          , ( cougar, back)
          , ( goat, front)
          )
        , ( ( cougar, front)
          , ( coyote, front)
          , ( ram, front)
          , ( goat, back)
          )
        , ( ( coyote, back)
          , ( coyote, front)
          , ( ram, back)
          , ( cougar, back)
          )
        , ( ( goat, back)
          , ( ram, back)
          , ( coyote, back)
          , ( cougar, front)
          )
        , ( ( cougar, front)
          , ( coyote, back)
          , ( coyote, back)
          , ( goat, back)
          )
        )

valid board =  
  let ( a, b, c, d, e, f, h, i, j) = unlift board :: (Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile)
  in east a `match` west b
      && east b `match` west c
      && east d `match` west e
      && east e `match` west f
      && east h `match` west i
      && east i `match` west j
      && south a `match` north d
      && south d `match` north h
      && south b `match` north e
      && south e `match` north i
      && south c `match` north f
      && south f `match` north j
      where match edge edge' = 
              let (pic, end) = unlift edge :: (Exp Int, Exp Int)
                  (pic', end') = unlift edge :: (Exp Int, Exp Int)
              in (pic == pic') && (end /= end')
            north t = let (n, _, _, _) = unlift t :: (Exp Edge, Exp Edge, Exp Edge, Exp Edge) in n
            east t = let (_, e, _, _) = unlift t :: (Exp Edge, Exp Edge, Exp Edge, Exp Edge) in e
            south t = let (_, _, s, _) = unlift t :: (Exp Edge, Exp Edge, Exp Edge, Exp Edge) in s
            west t = let (_, _, _, w) = unlift t :: (Exp Edge, Exp Edge, Exp Edge, Exp Edge) in w

rotate tile =
  let (a, b, c, d) = unlift tile :: (Exp Edge, Exp Edge, Exp Edge, Exp Edge)
  in
  lift (d, a, b, c) :: Exp (Edge, Edge, Edge, Edge)

main = do
  let result = CUDA.run $ allRotations
  print result

toList (a, b, c, d, e, f, g, h, i) = [a, b, c, d, e, f, g, h, i]
toTuple [a, b, c, d, e, f, g, h, i] = (a, b, c, d, e, f, g, h, i)


allRotations = filter (valid) $ generate (constant $ Z :. (4 P.^9::Int)) rotateIndex

rotateIndex ix = 
  let (a, b, c, d, e, f, g, h, i) = (unlift $ constant $ items :: (Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile, Exp Tile))
  in
  lift
  ( rot 1 a
  , rot 2 b
  , rot 3 c
  , rot 4 d
  , rot 5 e
  , rot 6 f
  , rot 7 g
  , rot 8 h
  , rot 9 i) :: Exp (Tile, Tile, Tile, Tile, Tile, Tile, Tile, Tile, Tile)
  where index :: Exp Int
        index = unindex1 ix
        orientations :: Exp Int
        orientations = A.constant 4
        magnitude :: Exp Int -> Exp Int
        magnitude power = (index `rem` (orientations A.^ power)) `div` (orientations A.^ (power-1))
        rot :: (Exp Int) -> Exp Tile -> Exp Tile 
        rot power tile = caseof (magnitude power) 
                         [ ((==0), tile)
                         , ((==1), rotate tile)
                         , ((==2), rotate $ rotate $ tile)
                         , ((==3), rotate $ rotate $ rotate $ tile)
                         ] $ tile
