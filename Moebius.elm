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

upperHalfPlaneDilation s =
  Moebius (Complex.Complex s 0) (Complex.Complex 0 0) (Complex.Complex 0 0) (Complex.Complex (1/s) 0)

upperHalfPlaneReflection =
  Moebius (Complex.Complex 0 0) (Complex.Complex 1 0) (Complex.Complex -1 0) (Complex.Complex 0 0)

-- maps a to 0
unitDiskTranslation a =
  Moebius (Complex.Complex 1 0) (Complex.negate a) (Complex.negate <| Complex.conjugate a) (Complex.Complex 1 0)

-- Map a to b by translation
unitDiskTranslatePoints a b =
  (unitDiskTranslation b |> inverse) `compose` (unitDiskTranslation a)

rotation theta = Moebius 
  (Complex.Complex (cos theta) (sin theta)) (Complex.Complex 0 0) (Complex.Complex 1 0)

-- divide by +-sqrt(det A)
normalize (Moebius a b c d) =
  let
    det = Complex.norm <| (a<*>d) `Complex.sub` (b<*>c)
    invDet2 = Complex.Complex (1/det) 0 --(Complex.Complex 1 0) `Complex.divide` (det <*> det)
  in Moebius (a <*> invDet2) (b <*> invDet2) (c <*> invDet2) (d <*> invDet2)
