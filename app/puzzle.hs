import Data.List
data Tile a = Tile { north :: Edge a
                   , east :: Edge a
                   , south :: Edge a
                   , west :: Edge a
                   }
            deriving (Show, Ord, Eq, Read)

data Edge a = Edge { picture :: !a 
                   , end :: {-# UNPACK #-} !End
                   }
            deriving (Show, Ord, Eq, Read)

data End = Front | Back
           deriving (Show, Ord, Eq, Read)

data Animals = Goat | Coyote | Cougar | Ram
               deriving (Show, Ord, Eq, Read)

items = [ Tile { north = Edge Goat Front
               , east = Edge Ram Back
               , south = Edge Cougar Front
               , west = Edge Coyote Back
               }
        , Tile { north = Edge Cougar Back
               , east = Edge Goat Front
               , south = Edge Ram Back
               , west = Edge Ram Front
               }
        , Tile { north = Edge Cougar Front
               , east = Edge Coyote Front
               , south = Edge Ram Back
               , west = Edge Goat Back
               }
        , Tile { north = Edge Coyote Back
               , east = Edge Goat Back
               , south = Edge Ram Front
               , west = Edge Cougar Front
               }
        , Tile { north = Edge Ram Front
               , east = Edge Goat Front
               , south = Edge Cougar Back
               , west = Edge Goat Front
               }
        , Tile { north = Edge Cougar Front
               , east = Edge Coyote Front
               , south = Edge Ram Front
               , west = Edge Goat Back
               }
        , Tile { north = Edge Coyote Back
               , east = Edge Coyote Front
               , south = Edge Ram Back
               , west = Edge Cougar Back
               }
        , Tile { north = Edge Goat Back
               , east = Edge Ram Back
               , south = Edge Coyote Back
               , west = Edge Cougar Front
               }
        , Tile { north = Edge Cougar Front
               , east = Edge Coyote Back
               , south = Edge Coyote Back
               , west = Edge Goat Back
               }
        ]

valid [ a, b, c
      , d, e, f
      , h, i, j
      ] =  east a `match` west b
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
      where match (Edge pic end) (Edge pic' end') = (pic == pic') && (end /= end')

rotate a = Tile { north = west a
                , east = north a
                , south = east a
                , west = south a
                }

allRotations = mapM (\a -> [ a
                           , rotate a
                           , rotate $ rotate a
                           , rotate $ rotate $ rotate a
                           ]
                    )

main = do
 putStrLn "Testing all arrangements"
 print $ filter (valid) 
       $ concatMap (permutations)
       $ allRotations items
