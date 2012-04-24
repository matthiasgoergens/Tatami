module Main where


import Control.Applicative
import Control.Arrow
import Data.List

main = do -- print $ fmap product $ powerset [1..5]
          mapM_ (print . (id &&& parts)) [2,4..]
       
parts n = let factors = unique . sort . fmap product . powerset . primeFactors $ n
          in takeWhile (uncurry (<=)) $ zip factors (reverse factors)

unique :: Eq a => [a] -> [a]
unique (a:xs@(b:_)) = (if a == b then id else (a:)) $ unique xs
unique x = x
    
powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
  where xss = powerset xs

        (/\/)        :: [a] -> [a] -> [a]
        []     /\/ ys = ys
        (x:xs) /\/ ys = x : (ys /\/ xs)



isPrime n = n > 1 &&
             foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
              True primes
 
primeFactors n | n > 1 = go n primes
   where
     go n ps@(p:ps')
         | p*p > n        = [n]
         | n `rem` p == 0 =  p : go (n `quot` p) ps
         | otherwise      =      go n ps'
                                

primes = 2 : filter isPrime [3,5..]