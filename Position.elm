module Position exposing(..)

import Point exposing(..)

type alias Position = { location : Point, scale : Point }

apply : Position -> Point -> Point
apply { location, scale } (x,y) = ((Tuple.first scale)*x + Tuple.first location, (Tuple.second scale)*y + Tuple.second location)

-- FIXME: Should return Maybe Position
inverse : Position -> Position
inverse { location, scale } =
  let invScale = Point.map (\x -> 1.0/x) scale
  in {
       location=-1.0 <*> (Tuple.first location / Tuple.first invScale, Tuple.second location / Tuple.second invScale),
       scale=invScale
     }


