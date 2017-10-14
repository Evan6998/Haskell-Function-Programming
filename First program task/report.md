# 函数程序设计实验一
`黄泳锋, 635051045@qq.com, 15331124, 软件工程`  
> 说明：  
试验中做了加法，除法，乘法的单元测试，以及乘法交换律和乘法结合律的测试。在不涉及除零的运算的测试均通过。
```hs
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
ratdiv (a, b) (c, d) = reduction (rattimes (a, b) (d, c))

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

infix 4 <==> 
(<==>) :: Fraction -> Fraction -> Bool
(<==>) (a, b) (c ,d) = fst (reduction (a, b)) == fst (reduction (c, d)) && snd (reduction (a, b)) == snd (reduction (c, d))

prop_ratplus_unit :: Fraction -> Property
prop_ratplus_unit (a,b) = b > 0 ==>(a, b) <+> (0,1) <==> (a, b)

prop_ratdiv_times_unit :: Fraction -> Fraction -> Property
prop_ratdiv_times_unit (a, b) (c, d) = b > 0 && c > 0 && d > 0 ==> (a, b) </> (c, d) <==> (a, b) <-*-> (d, c)

prop_rattimes_plus_distr :: Fraction -> Fraction -> Fraction -> Property
prop_rattimes_plus_distr (a,b) (c,d) (e,f) = b > 0 && d > 0 && f > 0 ==> (a,b) <-*-> ((c,d) <+> (e,f)) <==> ((a,b) <-*-> (c,d)) <+> ((a,b) <-*-> (e,f))

prop_rattimes_commutative_unit :: Fraction -> Fraction -> Fraction -> Property
prop_rattimes_commutative_unit (a, b) (c, d) (e, f) = b > 0 && d > 0 && f > 0 ==> ((a,b) <-*-> (c,d))<-*->(e,f) <==> (a,b)<-*->((c,d)<-*->(e,f))
```
