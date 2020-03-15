{- Assignment 5
 - Name: Talha Amjad
 - Date: 2019-12-01
 -}
module Assign_5 where

macid :: String
macid = "amjadt1"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Computes the approximation to a definite integral using the trapezoidal rule with n being the amount of
 partitions and g is the function that will represent the function being integrated.
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n 
                    | a > b = (-1)*(definiteIntegral b a g n)
                    | a == b = 0 -- when both lower and upper limits are the same then the area is equal to 0
                    | otherwise = deltaX * (g a + g(a + deltaX))/2 + definiteIntegralAux (a + deltaX) a b g n
                    where deltaX = (b-a)/(fromIntegral n)
definiteIntegralAux :: Double -> Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegralAux a a' b g n
    | (b-a <= 0.0000001) = 0
    | otherwise = deltaX * (g a + g(a + deltaX))/2 + definiteIntegralAux (a + deltaX) a' b g n
    where deltaX = (b-a') / fromIntegral n
{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - finds the approximation of the area between the 2 curves from [0,1]. As n approaches infinity, the area is 1
 - undefined when n <= 0

 -}
funH :: Integer -> Double
funH n = (definiteIntegral 0 1 (\x -> x ** (1/fromIntegral n))1000) - (definiteIntegral 0 1 (\x -> x ** (fromIntegral n)) 1000)

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - finds the approximation of the area between the 2 curves y = n^x and the x-axis from [-1,1]
 - as n approaches infinity, the function approaches infinity
 - function is undefined when n <= 0
 -}
funK :: Double -> Double
funK n = definiteIntegral (-1) 1 (\x -> n ** x) 1000

{--------------------------
---Test Cases and QuickCheck---
------------------------------
Function: definiteIntegral
Test Case Number: 1
Input: definiteIntegral 1 0 (\x -> x) 1000
Expected Output: -0.5000000000000006
Actual Output: -0.5000000000000006
--------------
Function: definiteIntegral
Test Case Number: 2
Input: definiteIntegral 1 1 (\x -> x**x) 1000
Expected Output: 0
Actual Output: 0.0
--------------
Function: definiteIntegral 
Test Case Number: 3
Input: definiteIntegral (1)(-10) (\x -> x**2) 1000
Expected Output: -333.66688850000935
Actual Output: -333.66688850000935
---QuickCheck for definiteIntegral----
Function: definiteIntegral
Property: 
dIprop1 :: Double -> Bool
dIprop1 n = definiteIntegral n n (\x -> x) 1000 == 0
Actual Test: OK, passed 100 tests.
------------
Function: definiteIntegral
Property: 
dIprop2 :: Integer -> Bool
dIprop2 n
        | n > 1 = definiteIntegral 0 10 (\x -> x^2 + 5) (n * 100) - definiteIntegral 0 10 (\x -> x^2 +5) (n * 1000) <= 1
        | otherwise = True
Actual Test: OK, passed 100 tests.
------------
Function: definiteIntegral
Property: 
dIprop3 :: Double -> Bool 
dIprop3 n
        | n >= 0 = abs(definiteIntegral 0 n (\x -> 2) 1000 - 2 * n) <= 0.00001
        | otherwise = abs(definiteIntegral 0 5 (\x -> n) 1000 - 5 * n) <= 0.00001
Actual Test: OK, passed 100 tests.
------------
==============================
--------Function 2-----------
Function: funH
Test Case Number: 1
Input: funH 1
Expected Output: 0
Actual Output: 0
-------------
Function: funH
Test Case Number: 2
Input: funH 0  
Expected Output: undefined
Actual Output: Infinity
-------------
Function: funH
Test Case Number: 3
Input: funH (999999999999999999999999)
Expected Output: undefined
Actual Output: -Infinity
----QuickCheck for function 2----
Function: funH
Property: 
funH1 :: Integer -> Bool
funH1 n
      | n > 0 = abs (funH n - 1) > abs (funH (fromIntegral n * 10) - 1)
      | otherwise = True
Actual Test: ok, Passed 100 tests.
=========================
-----Function 3----------
Function: funK
Test Case Number: 1
Input: funK 0
Expected Output: undefined
Actual Output: Infinity
-------------
Function: funK
Test Case Number: 2
Input: funK (-10000000000)
Expected Output: undefined
Actual Output: NaN
-------------
Function: funK
Test Case Number: 3
Input: funK (999999999999999999999)
Expected Output: REALLY SMALL NUMBER
Actual Output: 2.069680519823134e19
-------------
-----QuickCheck for function 3-------
Function: funK
property: 
funK1 :: Double -> Bool
funK1 n
      | n > 1 = funK n < funK (n * 10)
      | otherwise = True
Actual Tests: OK, passed 100 tests.
--------------------------------------}

