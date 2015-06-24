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
import Svg.Attributes exposing (..)
import Svg.Events
import Html exposing(Html)
import VirtualDom exposing(attribute)

import PlaneGeometry
import String
import Dict

import Point exposing(..) -- Point, <->, </>, <+> etc

type PlaneGeometryModel =
  Euclidean | HyperbolicDisc | HyperbolicHalfPlane

type alias Range = (Float, Float)


type GeometryElement =
  CircleSegment { center : Point, radius : Float, angle : Range }
  | LineSegment (Point, Point)
  | Marker Point
  | Image { source : String, bounds: Bounds.Bounds }

atanh x = 0.5*(logBase e)( (1+x)/(1-x) )

{-
Compass and Straightedge in the
Poincare Disk:
http://comp.uark.edu/~strauss/papers/hypcomp.pdf
-}
poincareArc (ax,ay) (bx,by) =
  let
    inverseA = (ax,ay) </> (ax*ax+ay*ay)
  in
    PlaneGeometry.circleFromThreePoints (ax,ay) (bx,by) inverseA

pathBetweenPoints : PlaneGeometryModel -> Point -> Point -> GeometryElement
pathBetweenPoints model (ax,ay) (bx,by) =
  case model of
    Euclidean -> LineSegment ((ax,ay), (bx,by))
    HyperbolicDisc ->
      case poincareArc (ax,ay) (bx,by) of
        Nothing -> LineSegment ((ax, ay), (bx,by))
        Just (c,r) ->
          let theta = angle <| (ax,ay) <-> c
              span = angleBetween ((ax,ay) <-> c) ((bx,by) <-> c)
          in CircleSegment { center=c, radius=r, angle=(theta, theta+span) }
    HyperbolicHalfPlane ->
      if ax == bx then
        LineSegment ((ax,ay), (bx,by))
      else
        let x = (bx*bx + by*by - ax*ax - ay*ay) / (2*(bx-ax))
            center = (x,0)
            r = distanceBetweenPoints Euclidean center (ax,ay)
            theta = atan2 ay (ax-x)
            theta'= atan2 by (bx-x)
        in CircleSegment { center=(x,0), radius=r, angle=(Basics.min theta theta', Basics.max theta theta') }

{-
  U_{z0,t}(z) = exp(i*t)* (z - z0)/(z - conj(z0))
  -- canonical map is from: i -> (0,0), (0,0) -> (0, -1)
  U_{i, pi/2}(z) = (i*z + 1)/(z+i)
-}

foldps : (a -> s -> (b,s)) -> (b,s) -> Signal a -> Signal b
foldps f bs aS = fst <~ foldp (\a (_,s) -> f a s) bs aS

catMaybes maybes = List.foldr (\x l -> case x of
  Nothing -> l
  Just x' -> x' :: l) [] maybes

type alias TouchMove = { id : Int, x : Float, y : Float, x_:Float,dx : Float, dy : Float}

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

--moebiusUpperHalfPlaneToPoincareDisk = Moebius (Complex 0 1) (Complex 1 0) (Complex 1 0) (Complex 0 1)
moebiusUpperHalfPlaneToPoincareDiskGeneral z0 t =
  let
    eit = Complex (cos t) (sin t)
  in
    Moebius.Moebius eit (Complex.negate <| eit `Complex.multiply` z0) (Complex 1 0) (Complex.negate <| Complex.conjugate z0)
moebiusUpperHalfPlaneToPoincareDisk = moebiusUpperHalfPlaneToPoincareDiskGeneral (Complex 0 100) (pi/2)
moebiusPoincareDiskToUpperHalfPlane = Moebius.inverse moebiusUpperHalfPlaneToPoincareDisk

{-
mapUpperHalfPlaneToPoincareDisk (x,y) =
  let
   (zi0, zi1) = (x, y+1)
   mod = zi0*zi0 + zi1*zi1
   down = (zi0 / mod, -zi1 / mod)
   complexMultiply (a,b) (c,d) = (a*c - b*d, a*d + b*c)
  in
    (-y+1, x) `complexMultiply` down
-}
mapUpperHalfPlaneToPoincareDisk (x,y) =
  let
    (Complex x' y') = Moebius.apply moebiusUpperHalfPlaneToPoincareDisk (Complex x y)
  in (x', y')

distanceBetweenPoints : PlaneGeometryModel -> Point -> Point -> Float
distanceBetweenPoints model (ax,ay) (bx,by) =
  case model of
    Euclidean ->
      let dx = ax-bx
          dy = ay-by
      in sqrt (dx*dx + dy*dy)
    HyperbolicDisc ->
      -- d(0, r) = ln( (1+r)/(1-r) )
      -- d(a, b) = 2*arctanh |(b-a)/(1-conj(a)*b)|

      atanh 0.5

type Entry = Geodesic Point Point
  | Circle Point Float (Maybe String)

type Tree a = Node a (List (Tree a))

generateSubTree depth arity =
  case depth of
    0 -> Node "" []
    _  ->
      let sub = generateSubTree (depth-1) arity
      in Node "" <| List.repeat arity sub

testTree = Node "0" [Node "00" [Node "000" [Node "001" []]]
                    ,Node "01" [Node "010" [Node "011" []]]
                    ,Node "02" []
                    ,Node "03" []
                    ,Node "04" []
                    ,Node "05" []
                    ,Node "06" []
                    ,Node "07" []
                    ,generateSubTree 4 2
                    ]

upperPlaneTravelToDirection (x,y) angle distance =
  let
    to = Moebius.upperHalfPlaneRotateAroundPoint (Complex x y) angle
           `Moebius.compose`
         Moebius.upperHalfPlaneTranslatePoints (Complex x y) (Complex 0 1)
    back = Moebius.upperHalfPlaneRotateAroundPoint (Complex x y) -angle
      `Moebius.compose`
      Moebius.upperHalfPlaneTranslatePoints (Complex 0 1) (Complex x y)

  in
    Moebius.apply back (Complex 0 (e^distance)) |> Complex.toPair

acosh x = logBase e (x + sqrt (x*x - 1))
entriesFromTree (Node a children) (x,y) radius =
      let angleIncrement = 2*pi/9.5
          minAngle = pi - angleIncrement * (List.length children |> toFloat)/2.0
          my = Circle (x, y) radius Nothing  -- TODO: pi/9.5 just seems visually fine
          others = List.concat <| List.indexedMap (\i child ->
              entriesFromTree child (upperPlaneTravelToDirection (x,y) (minAngle + (toFloat i)*angleIncrement) (2.0*radius)) (radius)) children
      in my :: others

-- no List.zip (only unzip)
zip : List a -> List b -> List (a,b)
zip listX listY =
  case (listX, listY) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    (  _  ,   _  ) -> []

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
upperPlaneTravelUp y distance = e^(distance + logBase e y)


-- define these in upper half plane
entries =
  entriesFromTree testTree (0,5) 1
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


{-
(a + ib, r) => (a + i sqrt(b^2 - r^2), b*tanh(r))

-}
euclideanCircleFromUnitDiskHyperbolicCircle (x,y_) r =
  let
    y = length (x,y_)
    div = -((1+(e^r))^2) + (-1+(e^r))^2 *y*y
    c' = ((-4*(e^r)*y) / div) <*> (normalize (x,y_))
    r' = ( (-1 + e^(2*r)) * (-1+y*y)) / div
  in
    (c', r')

-- z |-> (2|z|/(1+|z|^2))z
poincareToKlein p =
  let l = Point.lengthSquared p
  in (2*(sqrt l)/(1 + l)) <*> p

-- Do transformation in specified geometry model and output in euclidean coordinates
{-
transformEntry model transform entry =
  case entry of
    (Geodesic a b) ->
      let
        a' = Moebius.apply transform (Complex.fromPair a) |> Complex.toPair
        b' = Moebius.apply transform (Complex.fromPair b) |> Complex.toPair
      in Geodesic a' b'
    Circle c r ->
      case model of
{-
        HyperbolicDisc ->
          let
            c_disk = Moebius.apply transform (Complex.fromPair c)
            (c', r') = euclideanCircleFromUnitDiskHyperbolicCircle (Complex.toPair c_disk) r
          in Circle c' r'
-}
        _ ->
          let
            c' = Moebius.apply transform (Complex.fromPair c)
            r' = Moebius.apply transform (Complex.fromPair (c <+> (0,r))) `Complex.sub` c' |> Complex.norm
          in Circle (Complex.toPair c') r'
-}
transformEntry model transform entry = entry

sinh x = (e^x - e^(-x)) / 2
cosh x = (e^x + e^(-x)) / 2

generateGeometryFromEntry model transform entry =
  case entry of
    Geodesic a b ->
      case model of
        HyperbolicDisc ->
          let
            --transform' = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` transform
            transform' = transform
            (a',b') = (Moebius.apply transform' (Complex.fromPair a) |> Complex.toPair,
                       Moebius.apply transform' (Complex.fromPair b) |> Complex.toPair)
          in [pathBetweenPoints HyperbolicDisc a' b'
             ,Marker a'
             ,Marker b'
             ]
        _ -> let
             (a',b') = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                       Moebius.apply transform  (Complex.fromPair b) |> Complex.toPair)
             in [pathBetweenPoints model a' b', Marker a', Marker b']
    Circle (cx,cy) r content ->
      let
      (Circle (cx',cy') r' _) = case model of
        HyperbolicDisc -> -- Circle (mapUpperHalfPlaneToPoincareDisk (cx,cy)) r
          let
            --transform' = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` transform
            transform' = transform
            c' = Moebius.apply transform' (Complex cx cy) |> Complex.toPair
            (c_euclidean, r') = euclideanCircleFromUnitDiskHyperbolicCircle c' r
          in Circle c_euclidean r' Nothing

--        HyperbolicDisc -> transformEntry model moebiusUpperHalfPlaneToPoincareDisk entry
        -- (x,y,r) -> (x,y cosh r, y sinh r) = (x', y', r')
        HyperbolicHalfPlane ->
          let
            (Complex cx' cy') = Moebius.apply transform (Complex cx cy)
            -- try to travel upwards from y to y' by distance r:
            -- dist((x,y), (x, y')) = |log(y'/y)| = log(y'/y) = r
            -- <=> log(y') - log(y) = r
            -- <=> y' = exp(r + log(y))
--            r' = e^(r + logBase e cy')
            r' = r
          in Circle (cx',cy'*(cosh r')) (cy'*(sinh r')) Nothing --r' -- Circle (cx, cy*(cosh r)) (cy*(sinh r))
        _ -> entry
      extraElements = case content of
        Just src -> [Image {source=src, bounds=Bounds.fromCornerAndSize (cx',cy') (2*r', 2*r')}] --{left=cx-r', right=GeneralForm (fittedImage 1 1 src |> toForm |> scale (2*r')) (cx', cy')]
        Nothing -> []
      in [CircleSegment { center=(cx',cy'), radius=r', angle=(0, 2*pi) }] ++ extraElements


arc radius angleFrom angleTo =
  let
    step = (angleTo - angleFrom) / 30
  in
    List.map (\x -> (radius * cos (angleFrom+x*step), radius * sin (angleFrom+x*step))) [0..30] |> Graphics.Collage.path

constructSvg shape =
  case shape of
    -- TODO: use external CSS for these styles
    --CircleSegment {center, radius, angle} -> Svg.circle [ Svg.Attributes.style "fill:transparent; stroke:black; ", cx (fst center |> toString), cy (snd center |> toString), r (toString radius) ] []
    CircleSegment {center, radius, angle} ->
      if (((snd angle) - (fst angle) |> abs) - 2*pi |> abs) < 0.001
      then
        Svg.circle [ Svg.Attributes.style "fill:transparent; stroke:black; ", cx (fst center |> toString), cy (snd center |> toString), r (toString radius) ] []
      else
        let
          p r t = r <*> (cos t, sin t)
          (px,py) = center <+> (p radius (fst angle))
          (px',py') = center <+> (p radius (snd angle))
          largeFlag = "0" -- TODO
          pathString = "M" ++ (toString px) ++ "," ++ (toString py) ++ " A" ++
            (toString radius) ++ "," ++ toString radius ++ " 0 " ++ largeFlag ++ ",0 " ++
              toString px' ++ "," ++ toString py'

        in Svg.path [
               Svg.Attributes.d pathString
               , Svg.Attributes.style "fill:transparent; stroke:black;"
             ]
             []

    LineSegment (a,b) -> Svg.line [x1 (fst a |> toString), y1 (snd a |> toString),
                                   x2 (fst b |> toString), y2 (snd a |> toString)] []
    Marker (x0,y0) ->
      let s = 2
      in Svg.rect [x (x0-s |> toString), y (y0-s |> toString),
                   Svg.Attributes.width (2*s |> toString), Svg.Attributes.height (2*s |> toString), Svg.Attributes.style "fill:transparent;"] []
    Image {source, bounds} -> g [] [] -- TODO


construct scaling shape =
  case shape of
    CircleSegment {center, radius, angle} -> traced ({defaultLine | width <- 1}) (arc (scaling * radius) (fst angle) (snd angle) ) |> move (scaling <*> center)
    LineSegment (a,b) -> traced ({defaultLine | width <- 1}) (segment (scaling <*> a) (scaling <*> b))
    Marker a -> filled red (Graphics.Collage.circle 1.5) |> move (scaling <*> a)
    Image {source, bounds} -> fittedImage 1 1 source |> toForm |> Graphics.Collage.scale (scaling * Bounds.maxExtent bounds) |> move (scaling <*> (Bounds.corner bounds))
 --form pos -> form |> move (scaling <*> pos) |> scale scaling

width { left, right } = right-left
height { top, bottom } = top-bottom

{-
transformGeometry transform shape =
  case shape of
    CircleSegment {center, radius, angle} -> let
        center' = Moebius.apply transform (Complex.fromPair center)
        radius' = (Moebius.apply transform ((Complex.fromPair center) `Complex.add` (Complex 0 radius)))
          `Complex.sub` center' |> Complex.norm
      in CircleSegment { center=(Complex.toPair center'), radius=radius', angle=angle }
    LineSegment ((x,y),(x',y')) -> let
      (Complex a b) = Moebius.apply transform (Complex x y)
      (Complex a' b')  = Moebius.apply transform (Complex x' y')
      in LineSegment ((a,b),(a',b'))
    Marker a -> Marker (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair)
-}

transformGeometry scale shape =
  case shape of
    CircleSegment {center, radius, angle} -> CircleSegment {center=(scale*(fst center), scale*(snd center)), radius=scale*radius, angle=angle}
    LineSegment (a,b) -> LineSegment (scale <*> a, scale <*> b)
    Marker a -> Marker (scale <*> a)
    Image {source, bounds} -> Image {source=source, bounds=bounds}


upperHalfPlaneElementSvg entries transform =
  let
    data = List.concatMap (generateGeometryFromEntry HyperbolicHalfPlane transform) <| List.map (transformEntry HyperbolicHalfPlane transform) entries
    item = List.map constructSvg data |> g []
  in
    item

unitDiscElementSvg entries transform scale =
  let
    data = List.concatMap (generateGeometryFromEntry HyperbolicDisc transform) <| List.map (transformEntry HyperbolicDisc transform) entries
    item = List.map (constructSvg << transformGeometry scale) data |> g []
  in
    item

upperHalfPlaneElement transform =
  let
    data = List.concatMap (generateGeometryFromEntry HyperbolicHalfPlane transform) <| List.map (transformEntry HyperbolicHalfPlane transform) entries
    item = [
        List.map (construct 5) data |> group
        , traced (solid purple) (segment (-150/5, 0) (150/5, 0))
      ] |> group |> moveY (-150)
  in
    collage 300 300 [item]

upperHalfPlaneEuclideanElement transform =
  let
    data = List.concatMap (generateGeometryFromEntry Euclidean transform) <| List.map (transformEntry Euclidean transform) entries
    item = [
        List.map (construct 5) data |> group
        , traced (solid purple) (segment (-150, 0) (150, 0))
      ] |> group |> moveY (-150)
  in
    collage 300 300 [item]

unitDiscElement transform =
  let
    data = List.concatMap (generateGeometryFromEntry HyperbolicDisc transform) <| List.map (transformEntry HyperbolicDisc transform) entries
  in
    collage 300 300 [
      List.map (construct 150) data |> group
      , outlined (solid purple) (Graphics.Collage.circle 150)
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
  | UnitDiscFocus Box

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
         UnitDiscFocus box ->
           -- do stuff locally but return result in upper half plane
           let
             invTrans = inversePosition box.position
             discTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
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
                       discTransform |> Moebius.normalize)
                     |> Maybe.withDefault moeb

               Nothing ->
                 moebiusPoincareDiskToUpperHalfPlane
                   `Moebius.compose`
                 Moebius.unitDiskTranslatePoints (Complex x'' y'') (Complex x' y')
                   `Moebius.compose`
                 discTransform |> Moebius.normalize
                --rotationAngle = PlaneGeometry.angleBetweenLines (x',y') (x2',y2') (x'', y'') (x2'', y2'')
                --rotationAngle = Point.angleBetween (x2'-x', y2'-y') (x2''-x'', y2''-y'')
                --rotationAngle = Point.angleBetween (x2'-x', y2'-y') (x2''-x'', y2''-y'')
                --intersectionPoint = PlaneGeometry.lineLineIntersection (x',y') (x2',y2') (x'', y'') (x2'', y2'')

{-
                Maybe.withDefault moeb
                  <| Maybe.map (\x ->
                  Just p ->
                  Nothing -> moeb -- no rotation due to parallel lines, do nothing
                  -}
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

f {a,b} {a,b} = a-a

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

unitDiscBox = { upperHalfBox |
                  name <- "unit-disc"
                  , bounds <- Bounds.fromCornerAndSize (300,0) (300,300)
                  , position <- {location=(450, 150),scale=(1.0,-1.0) }
              }

boxes = [upperHalfBox, unitDiscBox]

main : Signal Html
main = Signal.map (\state ->
  let moeb = state.transformation
      -- touch events missing from Svg.Events
      onTouchStart = Svg.Events.messageOn "touchstart"
      focusMessage = Signal.message focus.address
      boundingBox = let (b :: rest) = List.map .bounds boxes
                    in List.foldr Bounds.union b rest
      discTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
      upperHalfSvg = upperHalfPlaneElementSvg entries moeb
      unitDiscSvg = unitDiscElementSvg entries discTransform 150
      gfxContent = svg [ version "1.1", x "0", y "0", viewBox (boundsToViewBox boundingBox),
        Svg.Attributes.width (Bounds.width boundingBox |> toString),
        Svg.Attributes.height (Bounds.height boundingBox |> toString)]
                   [g [class upperHalfBox.name, transform (positionToSvgTransform upperHalfBox.position)--, viewBox (boundsToViewBox upperHalfBox.bounds)
                       ,Svg.Events.onMouseDown (Signal.message focus.address (UpperhalfPlaneFocus upperHalfBox))
                       ,onTouchStart (Signal.message focus.address (UpperhalfPlaneFocus upperHalfBox))]
                       [upperHalfSvg]
                   ,g [class unitDiscBox.name, transform (positionToSvgTransform unitDiscBox.position)--, viewBox (boundsToViewBox unitDiscBox.bounds)
                       ,Svg.Events.onMouseDown (Signal.message focus.address (UnitDiscFocus unitDiscBox))
                       ,onTouchStart (Signal.message focus.address (UnitDiscFocus unitDiscBox))]
                       [unitDiscSvg, CircleSegment {radius=150, center=(0,0), angle=(0, 2*pi)} |> constructSvg ]]
  in
    Html.div [] [gfxContent
                 , Html.text (toString state.focus)
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
      discTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
  in flow down [
     flow right [
       upperHalfPlaneElement moeb
         |> Graphics.Input.hoverable
            (\x -> let msg = if x then UpperhalfPlaneFocus else NoFocus
                   in Signal.message focus.address msg)
       , spacer 10 1,
       upperHalfPlaneEuclideanElement moeb,
       spacer 10 1,
       unitDiscElement moeb
         |> Graphics.Input.hoverable
            (\x -> let msg = if x then UnitDiscFocus else NoFocus
                   in Signal.message focus.address msg)
     ]
     , show state.focus
     , show moeb

     ]) state

-}
