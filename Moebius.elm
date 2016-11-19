module Moebius exposing (..)

import Complex exposing(Complex)

type Moebius = Moebius Complex Complex Complex Complex

inverse (Moebius a b c d) =
  Moebius d (Complex.negate b) (Complex.negate c) a

-- do these have to be top level definitions? infix{l,r} does not work inside let
infixl 7 <*>
(<*>) a b = Complex.multiply a b
infixl 6 <+>
(<+>) a b = Complex.add a b

interpolate (Moebius a b c d) (Moebius a_ b_ c_ d_) t =
  let t_1 = 1-t
  in
  Moebius (Complex.realMultiply t_1 a <+> Complex.realMultiply t a_)
          (Complex.realMultiply t_1 b <+> Complex.realMultiply t b_)
          (Complex.realMultiply t_1 c <+> Complex.realMultiply t c_)
          (Complex.realMultiply t_1 d <+> Complex.realMultiply t d_)

compose (Moebius a b c d) (Moebius a_ b_ c_ d_) =
  Moebius (a<*>a_<+>b<*>c_) (a<*>b_<+>b<*>d_) (c<*>a_<+>d<*>c_) (c<*>b_<+>d<*>d_)


applyToPair moeb p =
  Complex.fromPair p |> apply moeb |> Complex.toPair

apply (Moebius a b c d) z =
  Complex.divide
    (Complex.add (Complex.multiply a z) b)
    (Complex.add (Complex.multiply c z) d)

identity = Moebius (Complex.Complex 1 0) (Complex.Complex 0 0) (Complex.Complex 0 0) (Complex.Complex 1 0)

upperHalfPlaneTranslation dx =
  Moebius (Complex.Complex 1 0) (Complex.Complex dx 0) (Complex.Complex 0 0) (Complex.Complex 1 0)

-- see "Drawing graphs in the Hyperbolic Plane"
upperHalfPlaneTranslatePoints (Complex.Complex a b) (Complex.Complex c d) =
  Moebius (Complex.Complex d 0) (Complex.Complex (b*c-a*d) 0) (Complex.Complex 0 0) (Complex.Complex b 0)

map f (Moebius a b c d) = Moebius (f a) (f b) (f c) (f d)
moebSum (Moebius a b c d) (Moebius a_ b_ c_ d_) =
  Moebius (a <+> a_) (b <+> b_) (c <+> c_) (d <+> d_)

exponential m =
  let
    next k fac macc iters =
      case iters of
        0 -> identity
        _ -> let
               fac_ = k*fac
               macc_ = (compose m macc)
               term = map (\x -> Complex.realMultiply (1.0/fac_) x) macc_
             in
               moebSum (next (k+1) fac_ macc_ (iters-1)) term
  in
    next 1 1 identity 150
 

logarithm : Moebius -> Moebius
logarithm (Moebius a b c d) = logarithm_
  <| Moebius (Complex.sub a Complex.one) b
              c (Complex.sub d Complex.one)
  
logarithm_ : Moebius -> Moebius
logarithm_ m =
  let
    next k macc iters =
      case iters of
        0 -> Moebius Complex.zero Complex.zero Complex.zero Complex.zero
        _ -> let
               k_ = toFloat <| if k % 2 /= 0 then k else -k
               macc_ = compose m macc
               term = map (\x -> Complex.realMultiply (1.0/k_) x) macc_
             in
               moebSum (next (k+1) macc_ (iters-1)) term
  in
    next 1 identity 150

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
    Moebius eit (Complex.negate <| Complex.multiply eit z0) Complex.one (Complex.negate <| Complex.conjugate z0)

-- maps a to 0
unitDiskTranslation a =
  Moebius (Complex.Complex 1 0) (Complex.negate a) (Complex.negate <| Complex.conjugate a) (Complex.Complex 1 0)

rotation theta = Moebius
  (Complex.Complex (cos theta) (sin theta)) Complex.zero Complex.zero Complex.one

-- Map a to b by translation
unitDiskTranslatePoints a b =
  compose (unitDiskTranslation b |> inverse) (unitDiskTranslation a)

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
        n = Complex.sub u2 u3
        d = Complex.sub u2 u1
      in
        Moebius n (Complex.multiply (Complex.negate u1) n) d (Complex.multiply (Complex.negate u3) d)
  in
    compose
      (inverse (mapThreePointsTo1Minus1Inf w1 w2 w3))
      (mapThreePointsTo1Minus1Inf z1 z2 z3)


-- TODO: explicit formula for composed transformation
unitDiskRotateAroundPoint p angle =
  compose (unitDiskTranslation p |> inverse)
    <| compose (rotation angle) (unitDiskTranslation p)

scaling s = Moebius (Complex.Complex s 0) Complex.zero Complex.zero Complex.one

-- Inversion in unit circle
inversion = Moebius Complex.zero Complex.one Complex.one Complex.zero
-- divide by +-sqrt(det A)
normalize (Moebius a b c d) =
  let
    det = sqrt <| Complex.norm <| Complex.sub (a<*>d) (b<*>c)
    invDet2 = Complex.Complex (1/det) 0
  in Moebius (a <*> invDet2) (b <*> invDet2) (c <*> invDet2) (d <*> invDet2)
