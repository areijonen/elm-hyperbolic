import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import String
import Tuple

import Mouse

import Moebius
import Bounds exposing (Bounds)
import Point exposing(..) -- Point, <->, </>, <+> etc
import HyperbolicGeometry
import PlaneGeometry
import Complex exposing (..)
import DrawableElement exposing (..)

type alias Position = { location : Point, scale : Point }

type alias Box = {
  name : String,
  bounds : Bounds, -- in parent cordinates
  position : Position -- in parent coordinates
}

type ElementFocus =
    UpperhalfPlaneFocus Box
  | PoincareDiskFocus Box
--  | KleinDiskFocus Box

type GeometryEntry = Geodesic Point Point
  | Circle Point Float (Maybe String)

type PlaneGeometryModel =
  Euclidean | PoincareDisk | HyperbolicHalfPlane | KleinDisk

type alias Model = {
  transformation : Moebius.Moebius,
  focus : Maybe ElementFocus,
  mousePosition : Maybe (Float, Float),
  entries : List GeometryEntry
}

emptyModel : Model
emptyModel = { transformation=Moebius.identity, focus=Nothing, mousePosition=Nothing, entries=[] }

poincareDiskScale = 150.0

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg =
  MouseMove (Float, Float)
  | ChangeFocus (Maybe ElementFocus)

init : (Model, Cmd Msg)
init = 
  let
    generateSubTree depth arity =
      case depth of
        0 -> Node "" []
        _  ->
          let sub = generateSubTree (depth-1) arity
          in Node "" <| List.repeat arity sub
    testTree = Node "0" [Node "00" [Node "000" [Node "001" [generateSubTree 2 2]]]
                    ,Node "01" [Node "010" [generateSubTree 2 2]]
                    ,generateSubTree 5 2
                    ]
    testEntries = entriesFromTree testTree (0,100) 0.5
  in
    ({emptyModel | entries=testEntries}, Cmd.none)

inversePosition { location, scale } =
  let invScale = Point.map (\x -> 1.0/x) scale
  in {
       location=-1.0 <*> (Tuple.first location / Tuple.first invScale, Tuple.second location / Tuple.second invScale),
       scale=invScale
     }

applyPosition { location, scale } (x,y) = ((Tuple.first scale)*x + Tuple.first location, (Tuple.second scale)*y + Tuple.second location)

calculateUpperhalfMove transformation box oldPosition position = 
  let
    invTrans = inversePosition box.position
    toLocal p = applyPosition invTrans p
    (x,y) = toLocal oldPosition
    (x_,y_) = toLocal position
  in
    transformation 
      |> Moebius.compose (Moebius.upperHalfPlaneTranslatePoints (Complex x y) (Complex x_ y_))
      |> Moebius.normalize

calculatePoincareDiskMove transformation box oldPosition position =
-- do stuff locally but return result in upper half plane
  let
    invTrans = inversePosition box.position
    diskTransform = Moebius.compose moebiusUpperHalfPlaneToPoincareDisk transformation
    diskTransformInv = Moebius.compose moebiusUpperHalfPlaneToPoincareDisk (Moebius.inverse transformation)
    -- converts p to unit disk coordinates 
    toLocal p = applyPosition invTrans p </> poincareDiskScale
    (x,y) = toLocal oldPosition
    (x_,y_) = toLocal position
  in
    List.foldr Moebius.compose Moebius.identity
      [moebiusPoincareDiskToUpperHalfPlane,
       Moebius.unitDiskTranslatePoints (Complex x y) (Complex x_ y_),
       moebiusUpperHalfPlaneToPoincareDisk,
       transformation]
    |> Moebius.normalize

update : Msg -> Model -> (Model, Cmd a)
update msg model =
  case msg of
    MouseMove position ->
      let
        f transformation prevPosition focus = 
          case focus of
            UpperhalfPlaneFocus box -> calculateUpperhalfMove transformation box prevPosition position 
            PoincareDiskFocus box -> calculatePoincareDiskMove transformation box prevPosition position
        transformation_ =
          Maybe.map2 (f model.transformation) model.mousePosition model.focus
            |> Maybe.withDefault model.transformation
      in
        ({model | mousePosition = Just position, transformation=transformation_}, Cmd.none)
    ChangeFocus focus -> ( {model | focus = focus, mousePosition = Nothing}, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Mouse.moves (\{x, y} -> MouseMove (toFloat x, toFloat y))

boundsToViewBox {left, right, top, bottom} =
  List.map toString [left,top,right,bottom] |> String.join " "

upperHalfBox : Box
upperHalfBox = { name = "upper-half"
               , bounds = Bounds.fromCornerAndSize (0,0) (300, 300)
               , position = { location=(150, 300), scale=(1,-1)}
               }

poincareDiskBox = { upperHalfBox |
                  name = "poincare-disk"
                  , bounds = Bounds.fromCornerAndSize (300,0) (300,300)
                  , position = {location=(450, 150),scale=(1.0,-1.0) }
              }

boxes = [upperHalfBox, poincareDiskBox]

positionToSvgTransform {location,scale} =
  let colWiseEntries = List.map toString [Tuple.first scale, 0, 0, Tuple.second scale, Tuple.first location, Tuple.second location]
  in "matrix(" ++ String.join " " colWiseEntries ++ ")"

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
      let svgs = List.map (constructSvg id) elements
      in Svg.g [] svgs
      
    Image {source, bounds} ->
       Svg.image [ Svg.Attributes.xlinkHref source,
                   Svg.Attributes.x (toString bounds.left),
                   Svg.Attributes.y (toString bounds.bottom),
                   Svg.Attributes.width (Bounds.width bounds |> toString ),
                   Svg.Attributes.height (Bounds.height bounds |> toString )
                 ] []
       -- g [] [] -- TODO

applyLod maxErr items =
  let
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

pathBetweenPoints : PlaneGeometryModel -> Point -> Point -> DrawableElement
pathBetweenPoints model (ax,ay) (bx,by) =
  case model of
    Euclidean -> LineSegment ((ax,ay), (bx,by))
    PoincareDisk ->
      case HyperbolicGeometry.poincareArc (ax,ay) (bx,by) of
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
            theta_= atan2 by (bx-x)
        in CircleSegment { center=(x,0), radius=r, angle=(Basics.min theta theta_, Basics.max theta theta_) }
    KleinDisk ->
      -- input assumed to be in poincare disk coordinates
      LineSegment (HyperbolicGeometry.poincareToKlein (ax,ay), HyperbolicGeometry.poincareToKlein (bx,by))
generateGeometryFromEntryUpperHalfPlane transform entry =
  case entry of
    Geodesic a b ->
      let
        (a_,b_) = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                   Moebius.apply transform (Complex.fromPair b) |> Complex.toPair)
      in pathBetweenPoints HyperbolicHalfPlane a_ b_ --, Marker a_, Marker b_

    Circle (cx,cy) r content ->
      let
        (Complex cx_ cy_) = Moebius.apply transform (Complex cx cy)
        r_ = r
        er = e^r_
        e_r = e^(-r_)
        {-
        sinh x = (e^x - e^(-x)) / 2
        cosh x = (e^x + e^(-x)) / 2
        -}
      in CircleSegment { center=(cx_,cy_*(er + e_r)/2), radius=cy_*(er - e_r)/2, angle=(0,2*pi) }

generateGeometryFromEntryPoincareDisk transform entry =
  case entry of
    Geodesic a b ->
      let
        (a_,b_) = (Moebius.apply transform (Complex.fromPair a) |> Complex.toPair,
                   Moebius.apply transform  (Complex.fromPair b) |> Complex.toPair)
      in pathBetweenPoints PoincareDisk a_ b_ --, Marker a_, Marker b_]
    Circle (cx,cy) r content ->
      let
        c_ = Moebius.apply transform (Complex cx cy) |> Complex.toPair
        (c_euclidean, r_) = HyperbolicGeometry.euclideanCircleFromPoincareDiskCircle c_ r
      in CircleSegment { center=c_euclidean, radius=r_, angle=(0, 2*pi) }

upperHalfPlaneElementSvg entries transform =
  let
    item =
      List.map (generateGeometryFromEntryUpperHalfPlane transform) entries
        |> applyLod 2
        |> List.indexedMap constructSvg
        |> Svg.g []
  in
    item

poincareDiskElementSvg entries transform scale =
  let
    data = List.map (generateGeometryFromEntryPoincareDisk transform) entries
      |> applyLod 0.01
    item = List.indexedMap (\i e -> constructSvg i (DrawableElement.transformElement scale e)) data |> Svg.g []
  in
    item

type Tree a = Node a (List (Tree a))


entriesFromTree (Node a children) (x,y) radius =
      let angleIncrement = 2*pi/6.8 --9.5
          minAngle = pi - angleIncrement * (List.length children |> toFloat)/2.0
          my = Circle (x, y) radius (Just "imgs/circle.png")
          others = List.concat <| List.indexedMap (\i child ->
            let
              (x_, y_) = HyperbolicGeometry.upperPlaneTravelToDirection (x,y) (minAngle + (toFloat i)*angleIncrement) (2.0*radius)
              line = Geodesic (x,y) (x_,y_)
            in
              line ::
              (entriesFromTree child (x_,y_) radius)) children
      in my :: others

moebiusUpperHalfPlaneToPoincareDisk = Moebius.upperHalfPlaneToPoincareDiskGeneral (Complex 0 100) (pi/2)
moebiusPoincareDiskToUpperHalfPlane = Moebius.inverse moebiusUpperHalfPlaneToPoincareDisk

view : Model -> Html Msg 
view model =
  let
      boundingBox = let boxBounds = List.map .bounds boxes
                    in case boxBounds of
                      (b :: rest) -> List.foldl Bounds.union b rest
                      rest -> Bounds.fromCenterAndExtents (0,0) (0,0)
      styleStr = "circle,path,line,polygon { fill: transparent; stroke: black;}" ++
        ".shape_0 { stroke: red; }" ++
        ".shape_1 { stroke: green; }" ++
        ".shape_2 { stroke: navy; }"
      moeb = model.transformation
      diskTransform = Moebius.compose moebiusUpperHalfPlaneToPoincareDisk moeb
      upperHalfSvg = upperHalfPlaneElementSvg model.entries moeb
      poincareDiskSvg = poincareDiskElementSvg model.entries diskTransform poincareDiskScale
      upperHalfElem = Svg.g [Svg.Attributes.clipPath <| "url(#clip-" ++ upperHalfBox.name ++ ")"]
          [Svg.g [Svg.Attributes.class upperHalfBox.name, Svg.Attributes.transform (positionToSvgTransform upperHalfBox.position)--, viewBox (boundsToViewBox upperHalfBox.bounds)
                ,Svg.Events.onMouseDown (ChangeFocus <| Just <| UpperhalfPlaneFocus upperHalfBox)
                ,Svg.Events.onMouseUp (ChangeFocus Nothing)
             ]
             [upperHalfSvg]]
      poincareDiskElem = Svg.g [Svg.Attributes.class poincareDiskBox.name, Svg.Attributes.transform (positionToSvgTransform poincareDiskBox.position)--, viewBox (boundsToViewBox poincareDiskBox.bounds)
            ,Svg.Events.onMouseDown (ChangeFocus <| Just <| PoincareDiskFocus poincareDiskBox)
            ,Svg.Events.onMouseUp (ChangeFocus Nothing)
          ]
          [poincareDiskSvg, CircleSegment {radius=poincareDiskScale, center=(0,0), angle=(0, 2*pi)} |> constructSvg 0 ]

      gfxContent = Svg.svg [ Svg.Attributes.version "1.1", Svg.Attributes.x "0", Svg.Attributes.y "0",
                         Svg.Attributes.viewBox (boundsToViewBox boundingBox),
        Svg.Attributes.width (Bounds.width boundingBox |> toString),
        Svg.Attributes.height (Bounds.height boundingBox |> toString)]
                   [
                   Svg.style [] [Html.text styleStr],
                   Svg.defs []
                     [
                       Svg.Lazy.lazy (\box ->
                         Svg.clipPath [Svg.Attributes.id ("clip-" ++ box.name)]
                         [Svg.rect [Svg.Attributes.x (Bounds.corner box.bounds |> Tuple.first |> toString)
                                   ,Svg.Attributes.y (Bounds.corner box.bounds |> Tuple.second |> toString)
                                   ,Svg.Attributes.width (Bounds.width box.bounds |> toString)
                                   ,Svg.Attributes.height (Bounds.height box.bounds |> toString)]
                                  []
                         ]
                       ) upperHalfBox,
                       Svg.g [Svg.Attributes.id "circle"] [Svg.circle [Svg.Attributes.style "fill:transparent; stroke:black;",
                           Svg.Attributes.r "5", Svg.Attributes.cx "0", Svg.Attributes.cy "0"] []]
                     ]
                   , upperHalfElem
                   , poincareDiskElem
--                   , kleinBoxElem
                   ]
  in
  div []
    [
      gfxContent
     ,Html.text (toString model.focus)
--     ,Html.text (toString model.transformation)
    ]
