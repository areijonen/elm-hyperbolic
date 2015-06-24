module Point where

type alias Point = (Float, Float)

(a,b) <+> (c,d) = (a+c, b+d)

--infixl 6 <->
(a,b) <-> (c,d) = (a-c, b-d)

--infixl 7 <*>
s <*> (a,b) = (s*a, s*b)

(a,b) </> (s) = (a/s, b/s)
(a,b) . (c,d) = a*c + b*d
rotate90 (a,b) = (-b, a)
length (a,b) = sqrt (a*a + b*b)
lengthSquared (a,b) = a*a + b*b
angle (x,y) = atan2 y x
dot (a,b) (c,d) = a*c + b*d
normalize v = v </> (length v)

map f (a,b) = (f a, f b)

angleBetween a b =
  let theta = acos <| (a `dot` b) / ((length a)*(length b))
      (ax,ay) = a
      (bx,by) = b
      sign = if ax*by - ay*bx < 0
             then -1
             else 1
  in
    sign * theta
