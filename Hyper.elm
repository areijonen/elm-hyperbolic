import Html exposing (Html, button, div, text)

import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy
import Tuple
import Mouse

import Moebius
import Bounds exposing (Bounds)
import Point exposing(..) -- Point, <->, </>, <+> etc
import HyperbolicGeometry
import Complex exposing (..)
import DrawableElement exposing (..)
import Position exposing (Position)
import SvgHelpers

type alias Box = {
  name : String,
  bounds : Bounds,
  position : Position
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

main : Program Never Model Msg
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

type Tree a = Node a (List (Tree a))

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
    
    testEntries = entriesFromTree testTree (0,100) 0.5
  in
    ({emptyModel | entries=testEntries}, Cmd.none)

calculateUpperhalfMove : Moebius.Moebius -> Box -> Point -> Point -> Moebius.Moebius
calculateUpperhalfMove transformation box oldPosition position = 
  let
    invTrans = Position.inverse box.position
    toLocal p = Position.apply invTrans p
    (x,y) = toLocal oldPosition
    (x_,y_) = toLocal position
  in
    transformation 
      |> Moebius.compose (Moebius.upperHalfPlaneTranslatePoints (Complex x y) (Complex x_ y_))
      |> Moebius.normalize

calculatePoincareDiskMove : Moebius.Moebius -> Box -> Point -> Point -> Moebius.Moebius
calculatePoincareDiskMove transformation box oldPosition position =
-- do stuff locally but return result in upper half plane
  let
    invTrans = Position.inverse box.position
    diskTransform = Moebius.compose moebiusUpperHalfPlaneToPoincareDisk transformation
    diskTransformInv = Moebius.compose moebiusUpperHalfPlaneToPoincareDisk (Moebius.inverse transformation)
    -- converts p to unit disk coordinates 
    toLocal p = Position.apply invTrans p </> poincareDiskScale
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

upperHalfBox : Box
upperHalfBox = { name = "upper-half"
               , bounds = Bounds.fromCornerAndSize (0,0) (300, 300)
               , position = { location=(150, 300), scale=(1,-1)}
               }

poincareDiskBox : Box
poincareDiskBox = { upperHalfBox |
                  name = "poincare-disk"
                  , bounds = Bounds.fromCornerAndSize (300,0) (300,300)
                  , position = {location=(450, 150),scale=(1.0,-1.0) }
              }

boxes : List Box
boxes = [upperHalfBox, poincareDiskBox]

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

generateGeometryFromEntryUpperHalfPlane : Moebius.Moebius -> GeometryEntry -> DrawableElement
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

generateGeometryFromEntryPoincareDisk : Moebius.Moebius -> GeometryEntry -> DrawableElement
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

upperHalfPlaneElementSvg : List GeometryEntry -> Moebius.Moebius -> Svg.Svg msg
upperHalfPlaneElementSvg entries transform =
  let
    item =
      List.map (generateGeometryFromEntryUpperHalfPlane transform) entries
        |> SvgHelpers.applyLod 2
        |> List.indexedMap SvgHelpers.drawableToSvg
        |> Svg.g []
  in
    item

poincareDiskElementSvg : List GeometryEntry -> Moebius.Moebius -> Float -> Svg.Svg msg
poincareDiskElementSvg entries transform scale =
  let
    data = List.map (generateGeometryFromEntryPoincareDisk transform) entries
      |> SvgHelpers.applyLod 0.01
    item = List.indexedMap (\i e -> SvgHelpers.drawableToSvg i (DrawableElement.transformElement scale e)) data |> Svg.g []
  in
    item


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
          [Svg.g [Svg.Attributes.class upperHalfBox.name, Svg.Attributes.transform (SvgHelpers.positionToTransform upperHalfBox.position)
                ,Svg.Events.onMouseDown (ChangeFocus <| Just <| UpperhalfPlaneFocus upperHalfBox)
                ,Svg.Events.onMouseUp (ChangeFocus Nothing)
             ]
             [upperHalfSvg]]
      poincareDiskElem = Svg.g [Svg.Attributes.class poincareDiskBox.name, Svg.Attributes.transform (SvgHelpers.positionToTransform poincareDiskBox.position)
            ,Svg.Events.onMouseDown (ChangeFocus <| Just <| PoincareDiskFocus poincareDiskBox)
            ,Svg.Events.onMouseUp (ChangeFocus Nothing)
          ]
          [poincareDiskSvg, CircleSegment {radius=poincareDiskScale, center=(0,0), angle=(0, 2*pi)} |> SvgHelpers.drawableToSvg 0 ]

      gfxContent = Svg.svg [ Svg.Attributes.version "1.1", Svg.Attributes.x "0", Svg.Attributes.y "0",
                         Svg.Attributes.viewBox (SvgHelpers.boundsToViewBox boundingBox),
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
                       ) upperHalfBox
                     ]
                   , upperHalfElem
                   , poincareDiskElem
                   ]
  in
  div []
    [
      gfxContent
     ,Html.text (toString model.focus)
--     ,Html.text (toString model.transformation)
    ]
