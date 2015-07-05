module GeneralizedCircle where

import Complex
import Moebius

{-
GeneralizedCircle a c d = hermitian matrix [a c#; c d]

Represents points on line or circle with complex points z satisfying:
  Azz# + Bz + Cz# + D = 0
where # is complex conjugation
-}
type GeneralizedCircle = GeneralizedCircle Float Complex.Complex Float

getCircleCenterRadius (GeneralizedCircle a c d) =
  if a == 0
  then Nothing
  else
    let
      det = a*d + Complex.normSquared c
    in
      if det < 0
      then Nothing
      else
        let
          center = (1/-a) `Complex.realMultiply` c
          radius = det / -(a*a)
        in
          Just (center, radius)

generalizedCircleFromCenterRadius center radius =
  let
    c = Complex.negate center
    d = Complex.normSquared center - radius*radius
  in
    GeneralizedCircle 1 c d

determinant (GeneralizedCircle a c d) = a*d - (Complex.normSquared c)

-- (M^-1)^T [a conj(c); c d] (M^-1)^*
transformWithMoebius moeb (GeneralizedCircle a c d) =
  let
    (Moebius ma mb mc md) = Moebius.inverse moeb
    (ma', mb', mc', md') = (Complex.conjugate ma, Complex.conjugate mb', Complex.conjugate mc', Complex.conjugate md')
    {-
    [ma mc][a c'][ma' mb']
    [mb md][c d ][mc' md']

    # = conj
    a'  = ma#*(a*ma + c * mc) + mc#(c# * ma + d * mc)
        = a*|ma|^2 + c*ma#*mc + c#*ma*mc# + d * |mc|^2
        = a*|ma|^2 + d*|mc|^2 + 2Re(c*ma#*mc)
    c'# = mb#*(a*ma + c * mc) + md#(c# * ma + d * mc)
    c'  = ma#*(a*mb + c * md) + mc#(c# * mb + d * md)

    d'  = mb#*(a*mb + c * md) + md#(c# * mb + d * md)
        = a*|mb|^2 + c*mb*md# + c#*mb*md# + d*|md|^2
        = a*|mb|^2 + d*|md|^2 + 2Re(c*mb*md#)
    -}
    b = Complex.conjugate c
    a' = a * (Complex.normSquared ma) + d * (Complex.normSquared mc) +
      c * (Complex.realPart <| ma' `Complex.multiply` mc)
    c' = (mb' `Complex.multiply` ((a `Complex.realMultiply` ma) `Complex.add` (c `Complex.multiply` md)))
           `Complex.add`
         (mc' `Complex.multiply` ((b `Complex.multiply` mb) `Complex.add` (d `Complex.realMultiply` md)))
    d' = a * (Complex.normSquared mb) + d * (Complex.normSquared md) +
      c * (Complex.realPart <| mb `Complex.multiply` md')
  in
    GeneralizedCircle a' c' d'
