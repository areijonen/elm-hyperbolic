module PlaneGeometry exposing (..)

import Point exposing(..)
import Complex

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

angleBetweenLines p1 p2 p3 p4 =
  let
    l1 = p2 <-> p1
    l2 = p4 <-> p3
  in
    (Point.dot l1 l2) / ((Point.length l1) * (Point.length l2)) |> acos

lineLineIntersection (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
  let
    divisor = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)
  in
    if divisor == 0
    --if (abs divisor) < 1e-5
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
