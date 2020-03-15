{- Assignment 5 Tests
 - Name: Talha Amjad
 - Date: 2019-12-01
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: definiteIntegral "
          quickCheck dIprop1 -- definiteIntegral property 1
          quickCheck dIprop2
          quickCheck dIprop3
          print "Performing Test 2: funH"
          quickCheck funH1
          print "Performing Test 3: funK"
          quickCheck funK1


--if the integral from [a,b] where both a and b are the same. The area will be 0 and output = 0
dIprop1 :: Double -> Bool
dIprop1 n = definiteIntegral n n (\x -> x) 1000 == 0

-- The bigger the n value, the more accurate the answer will become.
dIprop2 :: Integer -> Bool
dIprop2 n
    | n > 1 = definiteIntegral 0 10 (\x -> x^2 + 5) (n * 100) - definiteIntegral 0 10 (\x -> x^2 +5) (n * 1000) <= 1
    | otherwise = True

-- Basically, in the first gaurded statement, if n becomes a negative value, it will move on to the next statement
-- so that it can put the b value as a higher upper limit.
dIprop3 :: Double -> Bool 
dIprop3 n
    | n >= 0 = abs(definiteIntegral 0 n (\x -> 2) 1000 - 2 * n) <= 0.00001
    | otherwise = abs(definiteIntegral 0 2 (\x -> n) 1000 - 2 * n) <= 0.00001

-- n approaches infinit, the function will equal 1
funH1 :: Integer -> Bool
funH1 n
      | n > 0 = abs (funH n - 1) > abs (funH (fromIntegral n * 10) - 1)
      | otherwise = True
-- n approeaches infinity, function  approaches infinity, funK n is smaller than funK (n * 10)
funK1 :: Double -> Bool
funK1 n
      | n > 1 = funK n < funK (n * 10)
      | otherwise = True



