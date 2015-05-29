import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input
import Mouse
import Signal exposing(..)

import Complex exposing (..)
import Moebius exposing (..)

import Bounds

type alias Point = (Float, Float)

type PlaneGeometryModel = 
  Euclidean | HyperbolicDisc | HyperbolicHalfPlane

type alias Range = (Float, Float)


type GeometryElement =
  CircleSegment { center : Point, radius : Float, angle : Range }
  | LineSegment (Point, Point)
  | Marker Point
  | Image { source : String, bounds: Bounds.Bounds }

-- TODO precedence/assosiativity?
--infixl 6 <+>
(a,b) <+> (c,d) = (a+c, b+d)

--infixl 6 <->
(a,b) <-> (c,d) = (a-c, b-d)

--infixl 7 <*>
s <*> (a,b) = (s*a, s*b)

(a,b) </> (s) = (a/s, b/s)
(a,b) . (c,d) = a*c + b*d
rotate90 (a,b) = (-b, a)
length (a,b) = sqrt (a*a + b*b)
angle (x,y) = atan2 y x
dot (a,b) (c,d) = a*c + b*d
normalize v = v </> (length v)

angleBetween a b = 
  let theta = acos <| (a `dot` b) / ((length a)*(length b))
      (ax,ay) = a
      (bx,by) = b
      sign = if ax*by - ay*bx < 0
             then -1
             else 1
  in
    sign * theta

atanh x = 0.5*(logBase e)( (1+x)/(1-x) )

circleCircleIntersection (c1, r1) (c2,  r2) =
  let
    c_c = c2 <-> c1
    d = length c_c
  in
    if d > r1+r2 || d > r1+r2
    then [] 
    else
      let
        a = (r1*r1 - r2*r2 + d*d) / (2*d)
        h = sqrt (r1 - a)        
        p = c1 <+> ((a/d) <*> c_c)
        offset = (h/d) <*> c_c
      in
        [p <+> offset, p <-> offset]

lineLineIntersection (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
  let
    divisor = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  in
    if divisor == 0
    then
      Nothing
    else
      let
        e1 = x1*y2 - y1*x2
        e2 = x3*y4 - y3*x4 
      in
        Just <| (e1*(x3-x4) - e2*(x1-x2), e1*(y3-y4) - e2*(y1-y2)) </> divisor

circleFromThreePoints : Point -> Point -> Point -> Maybe (Point, Float)
circleFromThreePoints a b c =
  let
    o1 = 0.5 <*> (a <+> b)
    o2 = 0.5 <*> (b <+> c)
    s1 = o1 <+> rotate90 (b <-> a)
    s2 = o2 <+> rotate90 (c <-> b) 
    center = lineLineIntersection o1 s1 o2 s2
  in
    Maybe.map (\v -> (v, length (a <-> v))) center

{-
Compass and Straightedge in the
Poincare Disk:
http://comp.uark.edu/~strauss/papers/hypcomp.pdf
-}
poincareArc (ax,ay) (bx,by) =
  let
    inverseA = (ax,ay) </> (ax*ax+ay*ay)
  in
    circleFromThreePoints (ax,ay) (bx,by) inverseA

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
        in CircleSegment { center=(x,0), radius=r, angle=(min theta theta', max theta theta') }

{-
  U_{z0,t}(z) = exp(i*t)* (z - z0)/(z - conj(z0))
  -- canonical map is from: i -> (0,0), (0,0) -> (0, -1)
  U_{i,-1}(z) = (i*z + 1)/(z+i)
-}

--moebiusUpperHalfPlaneToPoincareDisk = Moebius (Complex 0 1) (Complex 1 0) (Complex 1 0) (Complex 0 1)
moebiusUpperHalfPlaneToPoincareDiskGeneral z0 t =
  let
    eit = Complex (cos t) (sin t)
  in
    Moebius eit (Complex.negate <| eit `Complex.multiply` z0) (Complex 1 0) (Complex.negate <| Complex.conjugate z0) 
moebiusUpperHalfPlaneToPoincareDisk = moebiusUpperHalfPlaneToPoincareDiskGeneral (Complex 0 1) (pi / 2)
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
  {-
  [
    Geodesic (-2,5) (2,5)
  , Geodesic (2,5) (2,7)
  , Geodesic (2,7) (-2,7)
  , Geodesic (-2,7) (-2,5)
  ]
  -}
  [Circle (0,3) 1 Nothing, Circle (0,3) 0.8 Nothing, Circle (0,3) 0.7 Nothing, Circle (0,3) 0.5 Nothing, Circle (0,3) 0.1 Nothing
  ,Circle (0, upperPlaneTravelUp 3 2) 1 (Just "imgs/circle.png")
-- ,Circle (-4,2.1) 2, Circle (0, 2.1) 2, Circle (4,2.1) 2, Circle (-2, 6.1) 2, Circle (2, 6.1) 2
  ]
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
            transform' = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` transform
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
            transform' = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` transform
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
    List.map (\x -> (radius * cos (angleFrom+x*step), radius * sin (angleFrom+x*step))) [0..30] |> path

construct scaling shape =
  case shape of
    CircleSegment {center, radius, angle} -> traced ({defaultLine | width <- 1}) (arc (scaling * radius) (fst angle) (snd angle) ) |> move (scaling <*> center)
    LineSegment (a,b) -> traced ({defaultLine | width <- 1}) (segment (scaling <*> a) (scaling <*> b))
    Marker a -> filled red (circle 1.5) |> move (scaling <*> a)
    Image {source, bounds} -> fittedImage 1 1 source |> toForm |> scale (scaling * Bounds.maxExtent bounds) |> move (scaling <*> (Bounds.corner bounds))
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
      , outlined (solid purple) (circle 150)
      ]

--keepWhen : Signal Bool -> a -> Signal a -> Signal a
drags : Signal (Maybe (Int,Int))
--drags = Mouse.position
--drags = Signal.sampleOn Mouse.isDown Mouse.position
drags = Signal.map (\(down, pos) -> if down then Just pos else Nothing) <|
        (,) <~ Mouse.isDown ~ Mouse.position

type alias DragMovement = { movement : Point, position : Point, initial : Bool }
-- (derivative, position)
dragMovement = Signal.foldp
  (\drag s -> 
    case drag of
      Nothing -> { movement=(0,0), position=(0,0), initial=True }
      Just pos -> if s.initial
                  then { movement=(0,0), position=pos, initial=False }
                  else { movement=pos <-> s.position, position=pos, initial=False})
  {movement=(0,0), position=(0, 0), initial=True} drags

type ElementFocus = NoFocus
  | UpperhalfPlaneFocus
  | UnitDiscFocus

focus = Signal.mailbox NoFocus

--    keepWhen Mouse.isDown (0,0) Mouse.position

transformation : Signal Moebius
transformation =
  let
    input = (,) <~ dragMovement ~ focus.signal
  in Signal.foldp
     (\({movement,position},focus) moeb ->
       let (x,y) = position
           (dx,dy) = movement
       in
       if dx == 0 && dy == 0
       then moeb
       else
       case focus of
         NoFocus -> moeb
         UpperhalfPlaneFocus ->
           let
             t = Moebius.upperHalfPlaneTranslation ( (toFloat dx) / 5.0 )
             (x',y') = ( (toFloat (x-150)), toFloat (300-y)) </> 5
             (x'',y'') = (toFloat ((x-dx)-150), toFloat (300-(y-dy))) </> 5
           in
             Moebius.upperHalfPlaneTranslatePoints (Complex x'' y'') (Complex x' y')
               `Moebius.compose` moeb |> Moebius.normalize
         UnitDiscFocus ->
           -- do stuff locally but return result in upper half plane
           let
             discTransform = moebiusUpperHalfPlaneToPoincareDisk `Moebius.compose` moeb
             (x',y') = (toFloat (x-150-620), toFloat (150-y)) </> 150
             (x'',y'') = (toFloat ((x-dx)-150-620), toFloat (150-(y-dy))) </> 150
           in
             moebiusPoincareDiskToUpperHalfPlane
               `Moebius.compose`
             Moebius.unitDiskTranslatePoints (Complex x'' y'') (Complex x' y') 
               `Moebius.compose`
             discTransform |> Moebius.normalize
     ) Moebius.identity input

type alias State = { transformation : Moebius, focus : ElementFocus }

state : Signal State
state = (\t f -> { transformation=t, focus=f }) <~ transformation ~ focus.signal

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

