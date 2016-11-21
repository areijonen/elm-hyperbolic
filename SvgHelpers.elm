module SvgHelpers exposing(..)

import DrawableElement exposing(..)
import Bounds exposing (Bounds)

import Svg
import Svg.Attributes
import Point exposing(..) -- Point, <->, </>, <+> etc

import Position exposing(Position)

positionToTransform : Position -> String
positionToTransform {location,scale} =
  let colWiseEntries = List.map toString [Tuple.first scale, 0, 0, Tuple.second scale, Tuple.first location, Tuple.second location]
  in "matrix(" ++ String.join " " colWiseEntries ++ ")"

boundsToViewBox : Bounds -> String
boundsToViewBox {left, right, top, bottom} =
  List.map toString [left,top,right,bottom] |> String.join " "

drawableToSvg : Int -> DrawableElement -> Svg.Svg msg
drawableToSvg id shape =
  -- TODO: some sensical use for id
  let class_name = toString (id % 3)
  in
  case shape of
    CircleSegment {center, radius, angle} ->
      let (a1,a2) = angle
      in
      if ((a2 - a1 |> abs) - 2*pi |> abs) < 0.001
      then
        let
          bounds = Bounds.fromCenterAndExtents center (radius, radius)
          (cxStr,cyStr) = (Tuple.first center |> toString, Tuple.second center |> toString)
          (cornerxStr,corneryStr) = (Tuple.first center - radius |> toString, Tuple.second center - radius |> toString)
          rStr = toString radius
          r2Str = toString (2*radius)
        in Svg.g [] <|
          [Svg.circle [
            Svg.Attributes.cx cxStr, Svg.Attributes.cy cyStr, Svg.Attributes.r rStr
            ,Svg.Attributes.class ("shape_" ++ toString (id % 3))
          ] []
{-
          ,Svg.image [
            Svg.Attributes.x cornerxStr,
            Svg.Attributes.y corneryStr,
            Svg.Attributes.width r2Str,
            Svg.Attributes.height r2Str,
            Svg.Attributes.xlinkHref "imgs/circle.png"] []
-}
          --,Svg.text [x cxStr, y cyStr] [Html.text "1"]
          ]
      else
        let
          p r t = r <*> (cos t, sin t)
          (px,py) = center <+> (p radius a1)
          (px_,py_) = center <+> (p radius a2)
          largeFlag = if a2-a1 > pi then "1" else "0"
          sweepFlag = if a1 < a2 then "1" else "0"
          pathString = "M" ++ (toString px) ++ "," ++ (toString py) ++ " A" ++
            (toString radius) ++ "," ++ toString radius ++ " 0 " ++ largeFlag ++ "," ++ sweepFlag ++ " " ++
              toString px_ ++ "," ++ toString py_

        in Svg.path [
               Svg.Attributes.d pathString
               ,Svg.Attributes.class class_name
             ]
             []

    LineSegment (a,b) -> Svg.line [Svg.Attributes.x1 (Tuple.first a |> toString), Svg.Attributes.y1 (Tuple.second a |> toString),
                                   Svg.Attributes.x2 (Tuple.first b |> toString), Svg.Attributes.y2 (Tuple.second b |> toString)
                                   ,Svg.Attributes.class class_name] []
    Polygon vertices ->
      let pointStr = List.map (\(x,y) -> (toString x) ++ "," ++ (toString y)) vertices |> String.join ","
      in Svg.polygon [Svg.Attributes.points pointStr, Svg.Attributes.class class_name] []
    Marker (x0,y0) ->
      let s = 2
      in Svg.rect [Svg.Attributes.x (x0-s |> toString), Svg.Attributes.y (y0-s |> toString),
                   Svg.Attributes.width (2*s |> toString), Svg.Attributes.height (2*s |> toString)] []
    MultiElement elements ->
      let svgs = List.map (drawableToSvg id) elements
      in Svg.g [] svgs
      
    Image {source, bounds} ->
       Svg.image [ Svg.Attributes.xlinkHref source,
                   Svg.Attributes.x (toString bounds.left),
                   Svg.Attributes.y (toString bounds.bottom),
                   Svg.Attributes.width (Bounds.width bounds |> toString ),
                   Svg.Attributes.height (Bounds.height bounds |> toString )
                 ] []
       -- g [] [] -- TODO

applyLod : Float -> List DrawableElement -> List DrawableElement
applyLod maxErr items =
  let
    catMaybes : List (Maybe a) -> List a
    catMaybes maybes = List.foldr (\x l -> case x of
      Nothing -> l
      Just x_ -> x_ :: l) [] maybes
    f x = Just <| case x of
      CircleSegment {center, radius, angle} ->
        if radius > maxErr
        then
          let
            begin = (radius <*> (cos <| Tuple.first angle, sin <| Tuple.first angle))
            end = (radius <*> (cos <| Tuple.second angle, sin <| Tuple.second angle))
            middlePoint = 0.5 <*> (begin <+> end)
            middleAngle = 0.5 * ((Tuple.first angle) + (Tuple.second angle))
            middleOnArc = radius <*> (cos middleAngle, sin middleAngle)
            -- maximum distance from line segment (begin, end) to arc between them
            error = middleOnArc <-> middlePoint |> Point.length
          in
            if error < maxErr
            then LineSegment (center <+> begin, center <+> end)
            else CircleSegment {center=center, radius=radius, angle=angle }
        else Marker center
      LineSegment (a,b) -> LineSegment (a,b)
      Marker a -> Marker a
      x -> x
  in
    List.map f items |> catMaybes
