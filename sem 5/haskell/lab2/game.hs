import System.Random
import Data.Bits
import Data.List
import Control.Monad 
import System.IO
import GHC.Base (VecElem(Int16ElemRep))
import System.Posix.Internals (lstat)
-- 1

-- game player1 player2 n m 
heaps n m = replicateM n (randomRIO (1, m))


heapSum (x:xs) = sum(x:xs)


genChoice (x:xs) = k
 where k = randomRIO (max(minimum (x:xs)) 1, maximum (x:xs))
 
findAndReduce :: (Ord t, Num t) => t -> [t] -> [t]
findAndReduce k [] = []
findAndReduce k (x:xs)
  | x >= k     = (x - k) : xs
  | otherwise = x : findAndReduce k xs

randStrategy :: [Int] -> IO [Int]
randStrategy (x:xs) = do
  k <- genChoice (x:xs)
  return $ findAndReduce k (x:xs)
nimSum (x:xs) = foldl xor x xs

replaceFirst oldElement newElement list = 
 case findIndex (== oldElement) list of
  Nothing -> list
  Just i -> take i list ++ [newElement] ++ drop (i + 1) list

xorStrategy xs = do
     let s = nimSum xs
     let a = maximum xs
     let updA = min (xor a s) (a-1)
     return (replaceFirst a updA xs)

decreaseIthElement i amount xs = 
  if i < 0 || i >= length xs
  then xs -- Return original list if i is invalid
  else take i xs ++ [xs !! i - amount] ++ drop (i + 1) xs

userChoice :: [Int] -> Int -> IO [Int]
userChoice (x:xs) n = do
     print "Choose a heap:"
     heap<-getLine
     let heap_ = read heap::Int
     print "Choose a number of stones:"
     stones<-getLine
     let stones_ = read stones::Int
     if heap_>n-1 || (x:xs) !! heap_ <stones_ 
     then do
          print "Wrong enter. Try again"
          userChoice (x:xs) n
     else do
          let lst = decreaseIthElement heap_ stones_ (x:xs)
          putStrLn $ "Current state: " ++ show lst -- Print the list
          return lst

gameUtil :: Integral t => [Int] -> Int -> t -> IO ()
gameUtil xs n it = if it > 100
     then putStrLn $ "It is draw. Current state: " ++ show xs
     else if heapSum xs == 0
          then 
          if mod it 2 == 0 
          then putStrLn $ "2 player wins" 
          else putStrLn $ "1 player wins"
     else do
          if mod it 2 == 0
          then do 
               lst <- userChoice xs n
               gameUtil lst n (it + 1) 
          else do
               lst <- xorStrategy xs
               putStrLn $ "Current state: " ++ show lst
               gameUtil lst n (it + 1)


game :: Int -> Int -> IO ()
game n m  = do
     lst <- (heaps n m)
     putStrLn $ "Beginning state: " ++ show lst
     gameUtil lst n 0


-- superUtil xs n it nS = if it > 100
--      then putStrLn $ "It is draw. Current state: " ++ show xs
--      else if heapSum xs == 0
--           then 
--           if mod it 2 == 0 
--           then putStrLn $ "2 player wins" 
--           else putStrLn $ "1 player wins"
--      else do
--           if mod (nS+it) 2 == 1
--                then do 
--                     lst <- xorStrategy xs
--                     putStrLn $ show (mod (it+nS+1) 2 +1) ++ " player. Current state: " ++ show lst
--                     superUtil lst n (it + 1) nS
--           else do 
--                lst <- randStrategy xs 
--                putStrLn $ show (mod (it+nS+1) 2 +1) ++" player. Current state: " ++ show lst
--                superUtil lst n (it + 1) nS

-- superGame :: Int -> Int -> IO ()
-- superGame n m  = do
--      lst <- (heaps n m)
--      putStrLn $ "Beginning state: " ++ show lst
--      if (nimSum lst) == 0
--          then superUtil lst n 0 0
--      else superUtil lst n 0 1

