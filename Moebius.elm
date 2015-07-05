module Moebius where

-- TODO: why doesn't this expose Complex
import Complex exposing(Complex)

type Moebius = Moebius Complex.Complex Complex.Complex Complex.Complex Complex

inverse (Moebius a b c d) =
  Moebius d (Complex.negate b) (Complex.negate c) a


-- do these have to be top level definitions? infix{l,r} does not work inside let
infixl 7 <*>
a <*> b = a `Complex.multiply` b
infixl 6 <+>
a <+> b = a `Complex.add` b

compose (Moebius a b c d) (Moebius a' b' c' d') =
  Moebius (a<*>a'<+>b<*>c') (a<*>b'<+>b<*>d') (c<*>a'<+>d<*>c') (c<*>b'<+>d<*>d')


applyToPair moeb p =
  Complex.fromPair p |> apply moeb |> Complex.toPair

apply (Moebius a b c d) z =
  ((a `Complex.multiply` z) `Complex.add` b) `Complex.divide`
    ((c `Complex.multiply` z) `Complex.add` d)

identity = Moebius (Complex.Complex 1 0) (Complex.Complex 0 0) (Complex.Complex 0 0) (Complex.Complex 1 0)

upperHalfPlaneTranslation dx =
  Moebius (Complex.Complex 1 0) (Complex.Complex dx 0) (Complex.Complex 0 0) (Complex.Complex 1 0)

-- see "Drawing graphs in the Hyperbolic Plane"
upperHalfPlaneTranslatePoints (Complex.Complex a b) (Complex.Complex c d) =
  Moebius (Complex.Complex d 0) (Complex.Complex (b*c-a*d) 0) (Complex.Complex 0 0) (Complex.Complex b 0)

upperHalfPlaneRotateAroundi theta =
  let (ct,st) = (cos theta, sin theta)
  in Moebius (Complex.Complex ct 0) (Complex.Complex st 0) (Complex.Complex -st 0) (Complex.Complex ct 0)

upperHalfPlaneRotateAroundPoint (Complex.Complex x y) theta =
  let (ct, st) = (cos (theta/2), sin (theta/2))
      a = y*ct - x*st
      b = (x*x+y*y) * st
      c = -st
      d = x*st + y*ct
  in Moebius (Complex.Complex a 0) (Complex.Complex b 0) (Complex.Complex c 0) (Complex.Complex d 0)

upperHalfPlaneDilation s =
  Moebius (Complex.Complex s 0) (Complex.Complex 0 0) (Complex.Complex 0 0) (Complex.Complex (1/s) 0)

upperHalfPlaneReflection =
  Moebius (Complex.Complex 0 0) (Complex.Complex 1 0) (Complex.Complex -1 0) (Complex.Complex 0 0)

-- maps z0 to (0,0) and rotates with angle t
upperHalfPlaneToPoincareDiskGeneral z0 t =
  let
    eit = Complex.Complex (cos t) (sin t)
  in
    Moebius eit (Complex.negate <| eit `Complex.multiply` z0) Complex.one (Complex.negate <| Complex.conjugate z0)

-- maps a to 0
unitDiskTranslation a =
  Moebius (Complex.Complex 1 0) (Complex.negate a) (Complex.negate <| Complex.conjugate a) (Complex.Complex 1 0)

rotation theta = Moebius
  (Complex.Complex (cos theta) (sin theta)) Complex.zero Complex.zero Complex.one

-- Map a to b by translation
unitDiskTranslatePoints a b =
  (unitDiskTranslation b |> inverse) `compose` (unitDiskTranslation a)

-- Forms the unique Möbius transformation mapping zi to wi
{-

n = (w2-w1)(z-z1)(z2-z3)
d = (w2-w3)(z-z3)(z2-z1)
k = n / d
t(z) = (w1 - k*w3) / (1 - k)

(z1,z2,z3) -> (1, -1, inf)
a,b,c,d = (z2-z3), -z1(z2-z3), (z2-z1), -z3(z2-z1)

-}


fromThreeCorrespondingPoints z1 z2 z3 w1 w2 w3 =
  let
    mapThreePointsTo1Minus1Inf u1 u2 u3 =
      let
        n = u2 `Complex.sub` u3
        d = u2 `Complex.sub` u1
      in
        Moebius n ((Complex.negate u1) `Complex.multiply` n) d ((Complex.negate u3) `Complex.multiply` d)
  in
    inverse (mapThreePointsTo1Minus1Inf w1 w2 w3)
      `compose`
    (mapThreePointsTo1Minus1Inf z1 z2 z3)


-- TODO: explicit formula for composed transformation
unitDiskRotateAroundPoint p angle =
  (unitDiskTranslation p |> inverse)
    `compose`
  (rotation angle)
    `compose`
  (unitDiskTranslation p)

scaling s = Moebius (Complex.Complex s 0) Complex.zero Complex.zero Complex.one

-- Inversion in unit circle
inversion = Moebius Complex.zero Complex.one Complex.one Complex.zero
-- divide by +-sqrt(det A)
normalize (Moebius a b c d) =
  let
    det = Complex.norm <| (a<*>d) `Complex.sub` (b<*>c)
    invDet2 = Complex.Complex (1/det) 0 --(Complex.Complex 1 0) `Complex.divide` (det <*> det)
  in Moebius (a <*> invDet2) (b <*> invDet2) (c <*> invDet2) (d <*> invDet2)
