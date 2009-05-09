id = \x . x

0 = \f . \x . x
1 = \f . \x . f x
s = \x . \y . \z . y (x y z)

pred = \n . \f . \x . n (\g . \h . h (g f)) (\u . x) (\u . u)
zero? = \n . n (\x . 0) 1

+ = \n1 . \n2 . \f . \x . n2 f (n1 f x)
* = \n1 . \n2 . \f . n1 (n2 f)

if = \c . \tf . \ff . (c tf ff) id

Y = \f . (\x . (\y . x x y)) (\x . (\y . x x y))

fact = Y (\f . \x . if (zero? x) (\_ . 1) (\_ . (* x (f (pred x)))))

------------------

fact (s s 1)
