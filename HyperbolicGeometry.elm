module HyperbolicGeometry exposing (..)

import Moebius
import Complex exposing(..)
import Point exposing(..)
-- from https://en.wikipedia.org/wiki/Beltrami%E2%80%93Klein_model
poincareToKlein u = (2 / (1 + (dot u u))) <*> u

upperPlaneTravelToDirection (x,y) angle distance =
  let
    back = Moebius.compose
      (Moebius.upperHalfPlaneRotateAroundPoint (Complex x y) -angle)
      (Moebius.upperHalfPlaneTranslatePoints (Complex 0 1) (Complex x y))
  in
    Moebius.apply back (Complex 0 (e^distance)) |> Complex.toPair

euclideanCircleFromPoincareDiskCircle (x,y_) r =
  let
    y = length (x,y_)
    div = -((1+(e^r))^2) + (-1+(e^r))^2 *y*y
    c_ = ((-4*(e^r)*y) / div) <*> (normalize (x,y_))
    r_ = ( (-1 + e^(2*r)) * (-1+y*y)) / div
  in
  {-
  radius is negative outside of the unit circle - taking absolute value will
  make it work "correctly" there too
  -}
    (c_, abs r_)
