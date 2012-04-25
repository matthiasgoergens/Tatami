module Main where


import Control.Applicative
import Control.Arrow
import Data.Function
import Data.List hiding (partition)

main = do -- print $ fmap product $ powerset [1..5]
--  print $ () [2,4..]
--  print $ tatami 14 14
        mapM_ print $ newBest $ fmap (id &&& nonTatami) [2,4..]
        
newBest xs@(x:_) = foldr (opBy (on (<) snd)) (const []) xs x

opBy :: (a -> a -> Bool) -> a -> (a -> [a]) -> a -> [a]
opBy lt x rest best = if x `lt` best then rest best else x:rest x

nonTatami :: Int -> Int       
nonTatami = (length . filter (not . uncurry tatami) . parts)

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

-- Throws lots of errors even in seemingly legitimate situations, to
-- help debug the rest of the code, which should confirm to certain expectations.
tatami :: Int -> Int -> Bool
tatami r s | r > s = error "Please rotate 90 degrees."
           | odd (r * s) = error "Given an odd size of room."
           | odd r = case r of
                       1 -> True
                       r -> partitionOdd r s
           | even r = case r of
                        2 -> True
                        r -> partitionEven r s
           | True = error ("Shouldn't happen.  " ++ show r ++ "is neither even nor odd.")

partitionEven r s = last $ take (s + 1) $ drop (r-1) ln'
    where
      -- stuff that ends in 1
      l1 = False : ln
      -- stuff that ends in something other than 1
      ln = replicate (r-1) False ++ True : zipWith (||) (drop 2 l1)  l1
      ln' = zipWith (||) ln l1
    
partitionOdd :: Int -> Int -> Bool
partitionOdd r s = last $ take (s + 1) $ drop r l
    where l = replicate r False ++ True : zipWith (||) (drop 2 l) l

          

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