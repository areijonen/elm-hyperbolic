module Complex where

type Complex = Complex Float Float

fromPair (x,y) = Complex x y
toPair (Complex x y) = (x,y)
negate (Complex a b) = Complex (-a) (-b)
multiply (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + b*c)
add (Complex a b) (Complex c d) = Complex (a+c) (b+d)
sub (Complex a b) (Complex c d) = Complex (a-c) (b-d)
divide (Complex a b) (Complex c d) =
  let r2 = c*c + d*d
  in Complex ((a*c + b*d)/r2) ((b*c - a*d)/r2)
conjugate (Complex a b) = Complex a -b
norm (Complex a b) = sqrt <| a*a + b*b
i = Complex 0 1
