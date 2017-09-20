module MyFraction where
import Test.QuickCheck

type Fraction = (Integer, Integer)

reduction :: Fraction -> Fraction
reduction (a, b) = (a `div` (gcd a b), b `div` (gcd a b))

ratplus :: Fraction -> Fraction -> Fraction
ratplus (a, b) (c, d) = reduction (a*d + b*c, b*d)

ratminus :: Fraction -> Fraction -> Fraction
ratminus (a, b) (c, d) = reduction (a*d - b*c, b*d)

rattimes :: Fraction -> Fraction -> Fraction
rattimes (a, b) (c, d) = reduction (a*c, b*d)

ratdiv :: Fraction -> Fraction -> Fraction
ratdiv (a, b) (c, d) = rattimes (a, b) (d, c)

ratfloor :: Fraction -> Integer
ratfloor (a, b) = floor (fromIntegral a / fromIntegral b)

ratfloat :: Fraction -> Float
ratfloat (a, b) = fromIntegral a / fromIntegral b

rateq :: Fraction -> Fraction -> Bool
rateq (a, b) (c, d) = if fst (reduction (a,b)) == fst (reduction (c, d)) && snd (reduction (a, b)) == snd (reduction (c, d)) then True else False

infix 5 <+>
(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (a, b) ( c, d) = ratplus (a, b) (c, d)

infix 5 <->
(<->) :: Fraction -> Fraction -> Fraction
(<->) (a, b) ( c, d) = ratminus (a, b) (c, d)

infix 6 <-*->
(<-*->) :: Fraction -> Fraction -> Fraction
(<-*->) (a,b) (c,d) = rattimes (a,b) (c,d)

infix 6 </>
(</>) :: Fraction -> Fraction -> Fraction
(</>) (a, b) ( c, d) = ratdiv (a, b) (c, d)

