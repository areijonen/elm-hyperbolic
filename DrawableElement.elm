module DrawableElement exposing (..)

import Point exposing(..)
import Bounds exposing(Bounds)

type DrawableElement =
  CircleSegment { center : Point, radius : Float, angle : (Float, Float) }
  | LineSegment (Point, Point)
  | Polygon (List Point)
  | Marker Point
  | Image { source : String, bounds: Bounds}
  | MultiElement (List DrawableElement)

transformElement scale shape =
  case shape of
    CircleSegment {center, radius, angle} -> CircleSegment {center=(scale*(Tuple.first center), scale*(Tuple.second center)), radius=scale*radius, angle=angle}
    LineSegment (a,b) -> LineSegment (scale <*> a, scale <*> b)
    Polygon vertices -> Polygon (List.map (\x -> scale <*> x) vertices)
    Marker a -> Marker (scale <*> a)
    Image {source, bounds} -> Image {source=source, bounds=bounds}
    MultiElement elems -> List.map (transformElement scale) elems |> MultiElement

