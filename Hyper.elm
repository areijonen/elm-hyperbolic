import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Mouse
import Signal exposing(..)

import Complex exposing (..)
import Moebius exposing (Moebius)
import Touch
import Bounds exposing (Bounds)

import Svg exposing (..)
import Svg.Attributes -- exposing (..)
import Svg.Events
import Svg.Lazy
import Html exposing(Html)
import VirtualDom exposing(attribute)

import PlaneGeometry
import HyperbolicGeometry
import String
import Dict

import Point exposing(..) -- Point, <->, </>, <+> etc

type GeometryEntry = Geodesic Point Point
  | Circle Point Float (Maybe String)

type Content = TextContent String | ImageContent String

type Tree a = Node a (List (Tree a))

type PlaneGeometryModel =
  Euclidean | PoincareDisk | HyperbolicHalfPlane | KleinDisk

type alias Range = (Float, Float)

type DrawableElement =
  CircleSegment { center : Point, radius : Float, angle : Range }
  | LineSegment (Point, Point)
  | Polygon (List Point)
  | Marker Point
  | Image { source : String, bounds: Bounds.Bounds }

type alias TouchMove = { id : Int, x : Float, y : Float, x_:Float,dx : Float, dy : Float}

foldps : (a -> s -> (b,s)) -> (b,s) -> Signal a -> Signal b
foldps f bs aS = fst <~ foldp (\a (_,s) -> f a s) bs aS

catMaybes maybes = List.foldr (\x l -> case x of
  Nothing -> l
  Just x' -> x' :: l) [] maybes

-- no List.zip (only unzip)
zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []

atanh x = 0.5*(logBase e)( (1+x)/(1-x) )
acosh x = logBase e (x + sqrt (x*x - 1))
sinh x = (e^x - e^(-x)) / 2
cosh x = (e^x + e^(-x)) / 2

{-
Compass and Straightedge in the
Poincare Disk:
http://comp.uark.edu/~strauss/papers/hypcomp.pdf
-}
poincareArc (ax,ay) (bx,by) =
  if (ax == 0 && ay == 0) || (bx == 0 && by == 0)
  then Nothing
  else
    let
      inverseA = (ax,ay) </> (ax*ax+ay*ay)
    in
      PlaneGeometry.circleFromThreePoints (ax,ay) (bx,by) inverseA

pathBetweenPoints : PlaneGeometryModel -> Point -> Point -> DrawableElement
pathBetweenPoints model (ax,ay) (bx,by) =
  case model of
    Euclidean -> LineSegment ((ax,ay), (bx,by))
    PoincareDisk ->
      case poincareArc (ax,ay) (bx,by) of
        Nothing -> LineSegment ((ax, ay), (bx,by))
        Just (c,r) ->
          let theta = angle <| (ax,ay) <-> c
              span = angleBetween ((ax,ay) <-> c) ((bx,by) <-> c)
          in CircleSegment { center=c, radius=r, angle=(theta, theta+span) }
    HyperbolicHalfPlane ->
      if (ax - bx |> abs) < 1e-5 then
        LineSegment ((ax,ay), (bx,by))
      else
        let x = (bx*bx + by*by - ax*ax - ay*ay) / (2*(bx-ax))
            r = (x-ax)*(x-ax) + ay*ay |> sqrt
            theta = atan2 ay (ax-x)
            theta'= atan2 by (bx-x)
        in CircleSegment { center=(x,0), radius=r, angle=(Basics.min theta theta', Basics.max theta theta') }
    KleinDisk ->
      -- input assumed to be in poincare disk coordinates
      LineSegment (HyperbolicGeometry.poincareToKlein (ax,ay), HyperbolicGeometry.poincareToKlein (bx,by))

touchDrags : Signal (List TouchMove)
touchDrags =
  let
    dictify : List Touch.Touch -> Dict.Dict Int Touch.Touch
    dictify touches = List.foldr (\x t -> Dict.insert x.id x t) Dict.empty touches
    step touches state =
      let
        -- id < 0 => mouse events in windows / google chrome
        state' = dictify <| List.filter (\x -> x.id >= 0) touches
        touchMoves = List.map (\touch->
          case Dict.get touch.id state' of
            Just touch' -> Just { id=touch'.id,
                                  x=toFloat touch'.x,
                                  y=toFloat touch'.y,
                                  x_=toFloat touch.x,
                                  dx=toFloat <| touch'.x-touch.x,
                                  dy=toFloat <| touch'.y-touch.y }
            Nothing -> Nothing) (Dict.values state) |> catMaybes
      in (touchMoves, state')
  in foldps step ([], Dict.empty) Touch.touches

moebiusUpperHalfPlaneToPoincareDisk = Moebius.upperHalfPlaneToPoincareDiskGeneral (Complex 0 100) (pi/2)
moebiusPoincareDiskToUpperHalfPlane = Moebius.inverse moebiusUpperHalfPlaneToPoincareDisk

generateSubTree depth arity =
  case depth of
    0 -> Node "" []
    _  ->
      let sub = generateSubTree (depth-1) arity
      in Node "" <| List.repeat arity sub

testTree = Node "0" [Node "00" [Node "000" [Node "001" [generateSubTree 2 2]]]
                    ,Node "01" [Node "010" [generateSubTree 2 2]]
--                    ,Node "02" []
                    {-
                    ,Node "03" []
                    ,Node "04" []
                    ,Node "05" []
                    ,Node "06" []
                    ,Node "07" []
                    -}
                    ,generateSubTree 4 2
                    ]


entriesFromTree (Node a children) (x,y) radius =
      let angleIncrement = 2*pi/6.8 --9.5
          minAngle = pi - angleIncrement * (List.length children |> toFloat)/2.0
          my = Circle (x, y) radius Nothing
          others = List.concat <| List.indexedMap (\i child ->
            let
              (x', y') = HyperbolicGeometry.upperPlaneTravelToDirection (x,y) (minAngle + (toFloat i)*angleIncrement) (2.0*radius)
              line = Geodesic (x,y) (x',y')
            in
              line ::
              (entriesFromTree child (x',y') radius)) children
      in my :: others


circularEntries r =
  let
    circlePoints = List.map (\t -> Complex (r*(cos t)) (r*(sin t))) (List.map (\x -> (x/20)*2*pi) [0..20])
    upperPoints = List.map (\p ->
       let (Complex a b) = p -- Moebius.apply moebiusPoincareDiskToUpperHalfPlane p
       in (a,b) <+> (0, 0)) circlePoints
  in
    List.map (\(a,b) -> Geodesic a b)
      <| zip upperPoints (Maybe.withDefault [] (List.tail upperPoints))

doCircle (cx,cy) r points =
  let
    circlePoints = List.map (\t -> (cx+r*(cos t),(cy+r*(sin t)))) (List.map (\x -> (x/points)*2*pi) [0..points])
  in
    List.map (uncurry Geodesic)
      <| zip circlePoints (Maybe.withDefault [] (List.tail circlePoints))

-- define these in upper half plane
entries =
  entriesFromTree testTree (0,100) 0.5
  {-
  [
    Geodesic (-2,5) (2,5)
  , Geodesic (2,5) (2,7)
  , Geodesic (2,7) (-2,7)
  , Geodesic (-2,7) (-2,5)
  ]
  -}
  {-
  [Geodesic (0, 1e-5) (0, 100)] ++
  [Geodesic (-1, 1e-5) (-1, 100)] ++
  [Geodesic (1, 1e-5) (1, 100)] ++
  [Circle (0,3) 1 Nothing, Circle (0,3) 0.8 Nothing, Circle (0,3) 0.6 Nothing, Circle (0,3) 0.4 Nothing, Circle (0,3) 0.2 Nothing, Circle (0,3) 0.03 Nothing
  ,Circle (0, upperPlaneTravelUp 3 2) 1 (Just "imgs/circle.png")
-- ,Circle (-4,2.1) 2, Circle (0, 2.1) 2, Circle (4,2.1) 2, Circle (-2, 6.1) 2, Circle (2, 6.1) 2
  ] -}
{-
  ++ doCircle (-4,2.1) 2 20
  ++ doCircle (0,2.1) 2 20
  ++ doCircle (4,2.1) 2 20
  ++ doCircle (-2,6.1) 2 20
  ++ doCircle (2,6.1) 2 20
  ++ [Geodesic (-4,2.1) (0,2.1)
     , Geodesic (0,2.1) (4,2.1)
     , Geodesic (-4,2.1) (-2, 6.1)
     , Geodesic (-2, 6.1) (2,6.1)
     , Geodesic (4,2.1) (2, 6.1)
     ]
-}

{-
  ++ circularEntries 0.1
  ++ circularEntries 1.2
  ++ circularEntries 5.3
  ++ circularEntries 8.4
  ++ circularEntries 13.5
  ++ circularEntries 16.6
  ++ circularEntries 21.7
-}
{-
  ++ List.map (\i -> Geodesic (5*i, 2) (5*(i+1), 2)) [-5..4]
  ++ List.map (\i -> Geodesic (5*i, 0.05) (5*(i+1), 0.05)) [-5..40]
  ++ List.map (\i -> Geodesic (5*i, 4) (5*(i+1), 4)) [-5..4]
-}
{-
  ++ List.concatMap (\x ->
       List.concatMap (\y ->
         [Geodesic (x,y) (x+1, y), Geodesic (x,y) (x, y+1)]
       ) [1..30]
     ) [-20..20]
-}
--  ++ List.map (\i -> Geodesic (0, 5*i+0.01) (0, 5*(i+1.01))) [0..5]



transformEntry model transform entry = entry

extraElements (Circle c r content) = case content of
  Just src -> [Image {source=src, bounds=Bounds.fromCornerAndSize c (2*r, 2*r)}] --{left=cx-r', right=GeneralForm (fittedImage 1 1 src |> toForm |> scale (2*r')) (cx', cy')]
  Nothing -> []

generateGeometryFromEntryEuclidean transform entry =
  case entry of
    Geodesic a b ->
      let
        (a',b') = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                   Moebius.apply transform (Complex.fromPair b) |> Complex.toPair)
      in pathBetweenPoints Euclidean a' b' --, Marker a', Marker b'

    Circle (cx,cy) r content ->
      let
        (Complex cx' cy') = Moebius.apply transform (Complex cx cy)
        r' = r
      in CircleSegment { center=(cx',cy'), radius=r', angle=(0,2*pi) }

generateGeometryFromEntryUpperHalfPlane transform entry =
  case entry of
    Geodesic a b ->
      let
        (a',b') = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                   Moebius.apply transform (Complex.fromPair b) |> Complex.toPair)
      in pathBetweenPoints HyperbolicHalfPlane a' b' --, Marker a', Marker b'

    Circle (cx,cy) r content ->
      let
        (Complex cx' cy') = Moebius.apply transform (Complex cx cy)
        r' = r
        er = e^r'
        e_r = e^(-r')
        {-
        sinh x = (e^x - e^(-x)) / 2
        cosh x = (e^x + e^(-x)) / 2
        -}
      in CircleSegment { center=(cx',cy'*(er + e_r)/2), radius=cy'*(er - e_r)/2, angle=(0,2*pi) }

generateGeometryFromEntryPoincareDisk transform entry =
  case entry of
    Geodesic a b ->
      let
        (a',b') = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                   Moebius.apply transform  (Complex.fromPair b) |> Complex.toPair)
      in pathBetweenPoints PoincareDisk a' b' --, Marker a', Marker b']
    Circle (cx,cy) r content ->
      let
        c' = Moebius.apply transform (Complex cx cy) |> Complex.toPair
        (c_euclidean, r') = HyperbolicGeometry.euclideanCircleFromPoincareDiskCircle c' r
      in CircleSegment { center=c_euclidean, radius=r', angle=(0, 2*pi) }

generateGeometryFromEntryKleinDisk transform entry =
  case entry of
    Geodesic a b ->
      let
        (a',b') = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair |> HyperbolicGeometry.poincareToKlein,
                   Moebius.apply transform (Complex.fromPair b) |> Complex.toPair |> HyperbolicGeometry.poincareToKlein)

      in LineSegment (a',b')
         --,Marker a',Marker b']

    Circle (cx,cy) r content ->
      let
        c' = Moebius.apply transform (Complex cx cy) |> Complex.toPair
        (c_euclidean, r') = HyperbolicGeometry.euclideanCircleFromPoincareDiskCircle c' r
        step = pi/15
        vertices = List.map (\x -> HyperbolicGeometry.poincareToKlein (c_euclidean <+> (r' * cos (x*step), r' * sin (x*step)))) [0..30]
      in
        Polygon vertices

generateGeometryFromEntry model transform entry =
  case model of
    HyperbolicHalfPlane -> generateGeometryFromEntryUpperHalfPlane transform entry
    PoincareDisk -> generateGeometryFromEntryPoincareDisk transform entry
    KleinDisk -> generateGeometryFromEntryKleinDisk transform entry

{-

Error: Invalid value for <path> attribute
d="
M-30.42929180110376,6.19720525949876
m -2.863834877476348,0
a 2.863834877476348,2.8638348774763480 1,0 5.727669754952696,0
a 2.863834877476348,2.8638348774763480 1,0 -5.727669754952696,0
Z
"patchObject @ Hyper.elm:13966applyProperties @ Hyper.elm:13925applyPatch @ Hyper.elm:14186applyPatch @ Hyper.elm:14381patchRecursive @ Hyper.elm:14356patch @ Hyper.elm:14334updateAndReplace @ Hyper.elm:15159draw @ Hyper.elm:10785drawCallback @ Hyper.elm:10771
-}
circleSegmentToSvgPathString center radius angle =
  let (a1,a2) = angle
  in
  if ((a2 - a1 |> abs) - 2*pi |> abs) < 0.001
  then
    let
      (px,py) = center <+> (radius,0)
      (px',py') = center <+> (-radius, 0)
      rStr = toString radius
      diamStr = toString (2*radius)
    in "M" ++ (toString (fst center)) ++ "," ++ (toString (snd center)) ++
      " m -" ++ rStr ++ ",0" ++
      " a " ++ rStr ++ "," ++ rStr ++ " 0 1,0 " ++ diamStr ++ ",0" ++
      " a " ++ rStr ++ "," ++ rStr ++ " 0 1,0 -" ++ diamStr ++ ",0 Z"
  else
    let
      p r t = r <*> (cos t, sin t)
      (px,py) = center <+> (p radius a1)
      (px',py') = center <+> (p radius a2)
      largeFlag = if a2-a1 > pi then "1" else "0"
      sweepFlag = if a1 < a2 then "1" else "0"
      pathString = "M" ++ (toString px) ++ "," ++ (toString py) ++ " A" ++
        (toString radius) ++ "," ++ toString radius ++ " 0 " ++ largeFlag ++ "," ++ sweepFlag ++ " " ++
          toString px' ++ "," ++ toString py'
    in pathString

compressItems items =
  let
    shapeToPathString shape =
      case shape of
        LineSegment ((x,y),(x',y'))
           -> "M" ++ (toString x) ++ "," ++ (toString y) ++
             " L" ++ (toString x') ++ "," ++ (toString y') ++ " Z"
        CircleSegment {center, radius, angle} ->
          circleSegmentToSvgPathString center radius angle
        _ -> ""
     {-
    LineSegment (a,b) = Ma Lb Z

    -}
    pathString = List.map shapeToPathString items |> String.join " "
    path = Svg.path [Svg.Attributes.d pathString] []
      {-
      List.map shapeToPathString items |>
      List.map (\x -> Svg.path [Svg.Attributes.d x] [])
      |> g []
      -}
  in
    path

constructSvg id shape =
  let class_name = toString (id % 3)
  in
  case shape of
    CircleSegment {center, radius, angle} ->
      let (a1,a2) = angle
      in
      if ((a2 - a1 |> abs) - 2*pi |> abs) < 0.001
      then
        let
          (cxStr,cyStr) = (fst center |> toString, snd center |> toString)
          rStr = toString radius
        in g [] <| [Svg.circle [
            Svg.Attributes.cx cxStr, Svg.Attributes.cy cyStr, Svg.Attributes.r rStr
            ,Svg.Attributes.class ("shape_" ++ toString (id % 3))
          ] []
          {-
          ,Svg.image [
            Svg.Attributes.x cxStr,
            Svg.Attributes.y cyStr,
            Svg.Attributes.width rStr,
            Svg.Attributes.height rStr,
            Svg.Attributes.xlinkHref "imgs/circle.png"] []
          -}
          --,Svg.text [x cxStr, y cyStr] [Html.text "1"]
          ]
      else
        let
          p r t = r <*> (cos t, sin t)
          (px,py) = center <+> (p radius a1)
          (px',py') = center <+> (p radius a2)
          largeFlag = if a2-a1 > pi then "1" else "0"
          sweepFlag = if a1 < a2 then "1" else "0"
          pathString = "M" ++ (toString px) ++ "," ++ (toString py) ++ " A" ++
            (toString radius) ++ "," ++ toString radius ++ " 0 " ++ largeFlag ++ "," ++ sweepFlag ++ " " ++
              toString px' ++ "," ++ toString py'

        in Svg.path [
               Svg.Attributes.d pathString
               ,Svg.Attributes.class class_name
             ]
             []

    LineSegment (a,b) -> Svg.line [Svg.Attributes.x1 (fst a |> toString), Svg.Attributes.y1 (snd a |> toString),
                                   Svg.Attributes.x2 (fst b |> toString), Svg.Attributes.y2 (snd b |> toString)
                                   ,Svg.Attributes.class class_name] []
    Polygon vertices ->
      let pointStr = List.map (\(x,y) -> (toString x) ++ "," ++ (toString y)) vertices |> String.join ","
      in Svg.polygon [Svg.Attributes.points pointStr, Svg.Attributes.class class_name] []
    Marker (x0,y0) ->
      let s = 2
      in Svg.rect [Svg.Attributes.x (x0-s |> toString), Svg.Attributes.y (y0-s |> toString),
                   Svg.Attributes.width (2*s |> toString), Svg.Attributes.height (2*s |> toString)] []
    Image {source, bounds} -> g [] [] -- TODO

construct scaling shape =
  let
    arc radius angleFrom angleTo =
      let
        step = (angleTo - angleFrom) / 30
      in
        List.map (\x -> (radius * cos (angleFrom+x*step), radius * sin (angleFrom+x*step))) [0..30] |> Graphics.Collage.path
  in case shape of
    CircleSegment {center, radius, angle} -> traced ({defaultLine | width <- 1}) (arc (scaling * radius) (fst angle) (snd angle) ) |> move (scaling <*> center)
    LineSegment (a,b) -> traced ({defaultLine | width <- 1}) (segment (scaling <*> a) (scaling <*> b))
    Marker a -> filled red (Graphics.Collage.circle 1.5) |> move (scaling <*> a)
    Image {source, bounds} -> fittedImage 1 1 source |> toForm |> Graphics.Collage.scale (scaling * Bounds.maxExtent bounds) |> move (scaling <*> (Bounds.corner bounds))

transformGeometry scale shape =
  case shape of
    CircleSegment {center, radius, angle} -> CircleSegment {center=(scale*(fst center), scale*(snd center)), radius=scale*radius, angle=angle}
    LineSegment (a,b) -> LineSegment (scale <*> a, scale <*> b)
    Polygon vertices -> Polygon (List.map (\x -> scale <*> x) vertices)
    Marker a -> Marker (scale <*> a)
    Image {source, bounds} -> Image {source=source, bounds=bounds}

{-

z = r*exp(it) + z0, z0 = r0*exp(it0)
z^2 = (r*exp(it) + z0)^2 = r^2*exp(2it) + 2*r0*exp(it0)*r*exp(it) + r0^2 * exp(2*it0)
    = r^2*exp(2it) + 2*r0*r*exp(i(t+t0)) + r0^2 * exp(2*it0)
    = (r*exp(it))(r*exp(it) + 2*r0*exp(it0)) + r0^2 * exp(2*it0)
-}

{-
transformGeometryMoebius moeb shape =
  case shape of
    CircleSegment {center, radius, angle} ->
      CircleSegment {center=Moebius.applyToPair moeb center, radius=scale*radius, angle=angle}
    LineSegment (a,b) ->
      LineSegment (Moebius.applyToPair moeb a, Moebius.applyToPair moeb b)
    Polygon vertices ->
      Polygon (List.map (\x -> Moebius.applyToPair moeb x) vertices)
    Marker a ->
      Marker (Moebius.applyToPair moeb a)
    Image {source, bounds} ->
      Image {source=source, bounds=bounds}
-}


applyLod maxErr items =
  List.map (\x ->
    Just <| case x of
      CircleSegment {center, radius, angle} ->
        if radius > maxErr
        then
          let
            begin = (radius <*> (cos <| fst angle, sin <| fst angle))
            end = (radius <*> (cos <| snd angle, sin <| snd angle))
            middlePoint = 0.5 <*> (begin <+> end)
            middleAngle = 0.5 * ((fst angle) + (snd angle))
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
    ) items |> catMaybes

upperHalfPlaneElementSvg entries transform =
  let
    item =
      List.map (generateGeometryFromEntryUpperHalfPlane transform) entries
        |> applyLod 2
        --|> compressItems
        |> List.indexedMap constructSvg
        |> g []
--        (\i e -> constructSvg i (generateGeometryFromEntryUpperHalfPlane transform e))
--        entries
  in
    item

poincareDiskElementSvg entries transform scale =
  let
    data = List.map (generateGeometryFromEntryPoincareDisk transform) entries
      |> applyLod 0.01
    --item = List.map (transformGeometry scale) data |> compressItems
    item = List.indexedMap (\i e -> constructSvg i (transformGeometry scale e)) data |> g []
  in
    item

kleinDiskElementSvg entries transform scale =
  let
    data = List.map (generateGeometryFromEntryKleinDisk transform) entries
    item =
      List.indexedMap (\i e -> constructSvg i (transformGeometry scale e)) data |> g []
  in
    item

upperHalfPlaneElement transform =
  let
    data = List.map (generateGeometryFromEntryUpperHalfPlane transform) entries
    item = [
        List.map (construct 5) data |> group
        , traced (solid purple) (segment (-150/5, 0) (150/5, 0))
      ] |> group |> moveY (-150)
  in
    collage 300 300 [item]

upperHalfPlaneEuclideanElement transform =
  let
    data = List.map (generateGeometryFromEntryEuclidean transform) entries
    item = [
        List.map (construct 5) data |> group
        , traced (solid purple) (segment (-150, 0) (150, 0))
      ] |> group |> moveY (-150)
  in
    collage 300 300 [item]

poincareDiskElement transform =
  let
    item = List.map (generateGeometryFromEntryPoincareDisk transform) entries
      |> List.map (construct 150) |> group
  in
    collage 300 300 [item
        ,outlined (solid purple) (Graphics.Collage.circle 150)
      ]

--keepWhen : Signal Bool -> a -> Signal a -> Signal a
drags : Signal (Maybe (Int,Int))
--drags = Mouse.position
--drags = Signal.sampleOn Mouse.isDown Mouse.position
drags = Signal.map (\(down, pos) -> if down then Just pos else Nothing) <|
        (,) <~ Mouse.isDown ~ Mouse.position

type alias DragMovement = { movement : Point, position : Point, initial : Bool }
-- (derivative, position)
dragMovement : Signal DragMovement
dragMovement = Signal.foldp
  (\drag s ->
    case drag of
      Nothing -> { movement=(0,0), position=(0,0), initial=True }
      Just (x,y) ->
        let pos = (toFloat x, toFloat y)
        in
          if s.initial
            then { movement=(0,0), position=pos, initial=False }
            else { movement=pos <-> s.position, position=pos, initial=False})
  {movement=(0,0), position=(0, 0), initial=True} drags

type alias Position = { location : Point, scale : Point }

type alias Box =
  {
    name : String,
    bounds : Bounds, -- in parent cordinates
    position : Position -- in parent coordinates
  }

type ElementFocus = NoFocus
  | UpperhalfPlaneFocus Box
  | PoincareDiskFocus Box
  | KleinDiskFocus Box

focus = Signal.mailbox NoFocus

-- calculate rotation caused by moving p1 to p1' and p2 to p2'
calculateRotation : Point -> Point -> Point -> Point -> Maybe (Float, Point)
calculateRotation p1 p1' p2 p2' =
  Maybe.map ((,) <| Point.angleBetween (p2 <-> p1) (p2' <-> p1')) <| PlaneGeometry.lineLineIntersection p1 p2 p1' p2'

transformation : Signal Moebius
transformation =
  let
    input = (\a b c -> (a,b,c)) <~ dragMovement ~ focus.signal ~ touchDrags
  in Signal.foldp
     (\({movement,position},focus,touches) moeb ->
       -- type alias TouchMove = { x : Float, y : Float, dx : Float, dy : Float}
       let (x,y,dx,dy) = case touches of
             h :: _ -> (h.x,h.y,h.dx,h.dy)
             _ -> (fst position, snd position, fst movement, snd movement)
           secondDrag = List.head (List.drop 1 touches)
       in
       if dx == 0 && dy == 0 && (List.length touches <= 1)
       then moeb
       else
       case focus of
         NoFocus -> moeb
         UpperhalfPlaneFocus box ->
           let
             --t = Moebius.upperHalfPlaneTranslation ( (dx) / 5.0 )
             invTrans = inversePosition box.position
             (x',y') = applyPosition invTrans (x,y)--( ((x-150)),  (300-y)) </> 5
             (x'',y'') = applyPosition invTrans (x-dx, y-dy) -- ( ((x-dx)-150),  (300-(y-dy))) </> 5
           in
             case secondDrag of
               Just touch ->
                 let
                   (x2,y2,dx2,dy2) = (touch.x, touch.y, touch.dx, touch.dy)
                   (x2',y2') = applyPosition invTrans (x2,y2)
                   (x2'',y2'') = applyPosition invTrans (x2-dx2, y2-dy2)
                 in
                   calculateRotation (x'',y'') (x',y') (x2'',y2'') (x2', y2')
                     |> Maybe.map (\(rotationAngle, p) ->
                       Moebius.upperHalfPlaneRotateAroundPoint (Complex.fromPair p) rotationAngle
                         `Moebius.compose`
                       moeb |> Moebius.normalize)
                     |> Maybe.withDefault moeb

               Nothing ->
                 Moebius.upperHalfPlaneTranslatePoints (Complex x'' y'') (Complex x' y')
                   `Moebius.compose` moeb |> Moebius.normalize
         PoincareDiskFocus box ->
           -- do stuff locally but return result in upper half plane
           let
             invTrans = inversePosition box.position
             diskTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
             toLocal p = applyPosition invTrans p </> 150.0
             (x',y') = toLocal (x,y)--( (x-150-620),  (150-y)) </> 150
             (x'',y'') = toLocal (x-dx, y-dy) -- ( ((x-dx)-150-620),  (150-(y-dy))) </> 150
           in
             case secondDrag of
               Just touch ->
                 let
                   (x2,y2,dx2,dy2) = (touch.x, touch.y, touch.dx, touch.dy)
                   (x2',y2') = toLocal (x2,y2)
                   (x2'',y2'') = toLocal (x2-dx2, y2-dy2)
                 in
                   calculateRotation (x'',y'') (x',y') (x2'',y2'') (x2', y2')
                     |> Maybe.map (\(rotationAngle, p) ->
                       moebiusPoincareDiskToUpperHalfPlane
                         `Moebius.compose`
                       Moebius.unitDiskRotateAroundPoint (Complex.fromPair p) rotationAngle
                         `Moebius.compose`
                       diskTransform |> Moebius.normalize)
                     |> Maybe.withDefault moeb

               Nothing ->
                 moebiusPoincareDiskToUpperHalfPlane
                   `Moebius.compose`
                 Moebius.unitDiskTranslatePoints (Complex x'' y'') (Complex x' y')
                   `Moebius.compose`
                 diskTransform |> Moebius.normalize
     ) Moebius.identity input

type alias State = { transformation : Moebius, focus : ElementFocus, drag : DragMovement, touches : List TouchMove }

state : Signal State
state = (\t f d touches-> { transformation=t, focus=f, drag=d, touches=touches }) <~ transformation ~ focus.signal ~ dragMovement ~ touchDrags

{-
[a 0 tx][c 0 tx']   [a*c  0    a*tx' + tx]
[0 d ty][0 b ty'] = [0    d*b  d*ty' + ty]
[0 0  1][0 0   1]   [0    0    1]
-}
composePosition p p' =
  let
    (x,y) = p.location
    (x',y') = p'.location
    (sx, sy) = p.scale
    (sx', sy') = p'.scale
    location' = (sx*x' + x, sy*y' + y)
    scale' = (sx*sx', sy*sy')
  in
    { location = location', scale = scale'}

inversePosition { location, scale } =
  let invScale = Point.map (\x -> 1.0/x) scale
  in {
       location=-1.0 <*> (fst location / fst invScale, snd location / snd invScale),
       scale=invScale
     }

applyPosition { location, scale } (x,y) = ((fst scale)*x + fst location, (snd scale)*y + snd location)

boundsToViewBox {left, right, top, bottom} =
  List.map toString [left,top,right,bottom] |> String.join " "

positionToSvgTransform {location,scale} =
  let colWiseEntries = List.map toString [fst scale, 0, 0, snd scale, fst location, snd location]
  in "matrix(" ++ String.join " " colWiseEntries ++ ")"

upperHalfBox : Box
upperHalfBox = { name = "upper-half"
               , bounds = Bounds.fromCornerAndSize (0,0) (300, 300)
               , position = { location=(150, 300), scale=(1,-1)}
               }

poincareDiskBox = { upperHalfBox |
                  name <- "poincare-disk"
                  , bounds <- Bounds.fromCornerAndSize (300,0) (300,300)
                  , position <- {location=(450, 150),scale=(1.0,-1.0) }
              }

kleinDiskBox = { poincareDiskBox |
                  name <- "klein-disk"
                  , bounds <- Bounds.fromCornerAndSize (600,0) (300,300)
                  , position <- {location=(750, 150),scale=(1.0,-1.0) }
              }


boxes = [upperHalfBox, poincareDiskBox, kleinDiskBox]

main : Signal Html
main = Signal.map (\state ->
  let moeb = state.transformation
      -- touch events missing from Svg.Events
      onTouchStart = Svg.Events.messageOn "touchstart"
      focusMessage = Signal.message focus.address
      boundingBox = let (b :: rest) = List.map .bounds boxes
                    in List.foldl Bounds.union b rest
      diskTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb

      upperHalfSvg = upperHalfPlaneElementSvg entries moeb
      poincareDiskSvg = poincareDiskElementSvg entries diskTransform 150
      kleinDiskSvg = kleinDiskElementSvg entries diskTransform 150

      upperHalfElem = g [Svg.Attributes.clipPath <| "url(#clip-" ++ upperHalfBox.name ++ ")"]
          [g [Svg.Attributes.class upperHalfBox.name, Svg.Attributes.transform (positionToSvgTransform upperHalfBox.position)--, viewBox (boundsToViewBox upperHalfBox.bounds)
              ,Svg.Events.onMouseDown (Signal.message focus.address (UpperhalfPlaneFocus upperHalfBox))
              ,onTouchStart (Signal.message focus.address (UpperhalfPlaneFocus upperHalfBox))]
             [upperHalfSvg]]

      poincareDiskElem = g [Svg.Attributes.class poincareDiskBox.name, Svg.Attributes.transform (positionToSvgTransform poincareDiskBox.position)--, viewBox (boundsToViewBox poincareDiskBox.bounds)
          ,Svg.Events.onMouseDown (Signal.message focus.address (PoincareDiskFocus poincareDiskBox))
          ,onTouchStart (Signal.message focus.address (PoincareDiskFocus poincareDiskBox))]
          [poincareDiskSvg, CircleSegment {radius=150, center=(0,0), angle=(0, 2*pi)} |> constructSvg 0 ]

      kleinBoxElem = g [Svg.Attributes.class kleinDiskBox.name, Svg.Attributes.transform (positionToSvgTransform kleinDiskBox.position)--, viewBox (boundsToViewBox kleinDiskBox.bounds)
          ,Svg.Events.onMouseDown (Signal.message focus.address (KleinDiskFocus kleinDiskBox))
          ,onTouchStart (Signal.message focus.address (KleinDiskFocus kleinDiskBox))]
          [kleinDiskSvg, CircleSegment {radius=150, center=(0,0), angle=(0, 2*pi)} |> constructSvg 0]

      styleStr = "circle,path,line,polygon { fill: transparent; stroke: black;}" ++
        ".shape_0 { stroke: red; }" ++
        ".shape_1 { stroke: green; }" ++
        ".shape_2 { stroke: navy; }"

      gfxContent = svg [ Svg.Attributes.version "1.1", Svg.Attributes.x "0", Svg.Attributes.y "0",
                         Svg.Attributes.viewBox (boundsToViewBox boundingBox),
        Svg.Attributes.width (Bounds.width boundingBox |> toString),
        Svg.Attributes.height (Bounds.height boundingBox |> toString)]
                   [
                   Svg.style [] [Html.text styleStr],
                   Svg.defs []
                     [
                       Svg.Lazy.lazy (\box ->
                         Svg.clipPath [Svg.Attributes.id ("clip-" ++ box.name)]
                         [Svg.rect [Svg.Attributes.x (Bounds.corner box.bounds |> fst |> toString)
                                   ,Svg.Attributes.y (Bounds.corner box.bounds |> snd |> toString)
                                   ,Svg.Attributes.width (Bounds.width box.bounds |> toString)
                                   ,Svg.Attributes.height (Bounds.height box.bounds |> toString)]
                                  []
                         ]
                       ) upperHalfBox,
                       g [Svg.Attributes.id "circle"] [Svg.circle [Svg.Attributes.style "fill:transparent; stroke:black;",
                           Svg.Attributes.r "5", Svg.Attributes.cx "0", Svg.Attributes.cy "0"] []]
                     ]
                   , upperHalfElem
                   , poincareDiskElem
                   , kleinBoxElem
                   ]
  in
    Html.div [] [gfxContent
                 ,Html.text (toString state.focus)
    {-
                 , Html.text <| (toString state.focus) ++ (toString state.drag) ++ (toString <| mapPointToBox upperHalfBox state.drag.position)
                 , Html.text (boundsToViewBox boundingBox)
--                 , Html.text << toString <| applyPosition (inversePosition upperHalfBox.position) state.drag.position
                 , Html.text <| toString state.touches
                 -}
                 ]
    ) state

{-
main : Signal Element
main = Signal.map (\state ->
  let moeb = state.transformation
      diskTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
  in flow down [
     flow right [
       upperHalfPlaneElement moeb
         |> Graphics.Input.hoverable
            (\x -> let msg = if x then UpperhalfPlaneFocus upperHalfBox else NoFocus
                   in Signal.message focus.address msg)
       , spacer 10 1,
       upperHalfPlaneEuclideanElement moeb,
       spacer 10 1,
       PoincareDiskElement moeb
         |> Graphics.Input.hoverable
            (\x -> let msg = if x then PoincareDiskFocus poincareDiskBox else NoFocus
                   in Signal.message focus.address msg)
     ]
     , show state.focus
     , show moeb

     ]) state
-}
