module GS

where

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

divides k n = n `mod` k == 0

prime0 n | n <  1    = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

-- Learned I need commas to seperate list elements. (>.<)
myRest :: [Int] -> [Int]
myRest [] = []
myRest (x:xs) = xs

removeFst :: (Eq a) => a -> [a] -> [a]
removeFst _ [] = []
removeFst m (x:xs) | x == m    = xs
                   | otherwise = x:(removeFst m xs)

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt2 xs

-- The case of an empty list doesn't have to be handled
-- because this is just a helper for sorting a non-empty list.
mnmInt :: [Int] -> Int
mnmInt [] = maxBound :: Int
mnmInt (x:xs) | x < mnmInt xs = x
              | otherwise     = mnmInt xs

-- I didn't figure this one out till I saw that the built in 
-- "min" function has the same problem as my original attempt.
mnmInt2 :: [Int] -> Int
mnmInt2 [] = maxBound :: Int
mnmInt2 (x:xs) = min x $ mnmInt2 xs

myLen :: [a] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

-- Write function for counting number of occurences of char
count :: Char -> String -> Int
count _ "" = 0
count a (x:xs) | a == x    = 1 + count a xs
               | otherwise = count a xs

-- do the following transformation: 1234 -> 1223334444
blowup :: String -> String
blowup x = myfmap dupChar $ enumerate x

myfmap :: (t -> [a]) -> [t] -> [a]
myfmap _ [] = []
myfmap f (x:xs) = f x ++ myfmap f xs

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

dupChar :: (Int, Char) -> String
dupChar (0,y) = y : []
dupChar (x,y) = y : dupChar (x-1, y)

-- Sort a string
srtString :: String -> String
srtString "" = ""
srtString xs = m : (srtString (removeFst m xs)) where m = mnmChar xs

myMinimum :: (Bounded a, Ord a) => [a] -> a
myMinimum [] = maxBound
myMinimum (x:xs) = min x $ myMinimum xs

-- Added after I realised I'm stupid
myMinimum2 :: (Ord a) => [a] -> a
myMinimum2 [] = error "empty list"
myMinimum2 [x] = x
myMinimum2 (x:xs) = min x $ myMinimum2 xs

mnmChar :: String -> Char
mnmChar xs = myMinimum xs

-- Terrible that a generic sort needs boundedness....
mySort :: (Ord a, Bounded a) => [a] -> [a]
mySort [] = []
mySort xs = m : (mySort (removeFst m xs)) where m = myMinimum xs

-- Also added after I got rid of the bounded assumption
mySort2 :: (Ord a) => [a] -> [a]
mySort2 [] = []
mySort2 xs = m : (mySort2 (removeFst m xs)) where m = myMinimum2 xs

-- I keep checking with :t if these things are in Prelude
prefix :: String -> String -> Bool
prefix xs ys = isEmpty $ filter (\(x,y) -> x /= y) $ zip xs ys

-- I cry when I see this
isEmpty :: [a] -> Bool
isEmpty [] = 1 == 1 -- don't know how to return true.
isEmpty _  = 1 == 2 -- and obviously false is also a problem.

-- We continue
factors :: Integer -> [Integer]
factors n | n <  1    = error "argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p) where p = ld n

-- Okay some obvious exercises from the book
bookLengths = map myLen

sumLengths xs = foldr (+) 0 $ bookLengths xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs) | p x       = x : filter p xs
                  | otherwise = filter p xs

primes1 :: [Integer]
primes1 = 2 : filter prime [3..]

-- Okay I am just so retardedly stupid see comment for ldp
prime :: Integer -> Bool
prime n | n <  1    = error "not a pos int"
        | n == 1    = False -- wut, didn't work above. TODO
        | otherwise = ldp n == n

-- It was at this point that I realized that I was only reading half the book
-- zathura was zoomed into one page but my pdf has two pages side by side so I
-- only read half the pages while filling in gaps by myself (thinking of them as
-- 'exercises for the reader') until I accidentally pressed 'h' while having
-- zathura focused and scrolled into the missing pages.
ldp :: Integer -> Integer
ldp = ldpf primes1

-- missing functions and exercises
average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

bookPrefix :: String -> String -> Bool
bookPrefix [] ys         = True
bookPrefix (x:xs) []     = False
bookPrefix (x:xs) (y:ys) = (x==y) && bookPrefix xs ys

-- substring exercise
substring :: String -> String -> Bool
substring xs (y:ys) | myLen xs > myLen (y:ys) = False
                    | prefix xs (y:ys)        = True
                    | otherwise               = substring xs ys

-- ok back to where I was
primes0 :: [Integer]
primes0 = filter prime0 [2..]

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

-- Now that I have both pages this became too easy again
-- Infinite recursion examples:
h1 :: Integer -> Integer
h1 0 = 0
h1 x = 2 * (h1 x)

h2 :: Integer -> Integer
h2 0 = 0
h2 x = h2 (x+1)

-- He recommended a book: Discrete mathematics using a computer
-- I'm doing a few exercises from that book in this section
-- This is because chapter 1 finishes with some references and I won't
-- do the whole of that other book just a few things.


-- Chapter 3 - Recursion

-- Ex 15
intersection :: Eq a => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x:xs) ys | in1 x ys   = x : intersection xs ys
                       | otherwise = intersection xs ys

in1 :: Eq a => a -> [a] -> Bool
in1 _ [] = False
in1 r (x:xs) | r == x    = True
             | otherwise = in1 r xs

-- Ex 16
isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = xs == intersection xs ys

-- Ex 17
isSorted :: Ord a => [a] -> Bool
isSorted xs = xs == mySort2 xs

-- Ex 18 = References book and also worthless

-- Ex 19
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x:xs) = myLast xs

-- Ex 20
splitFraction :: String -> [Integer]
splitFraction s = map read (words (dirtyFix s))

dirtyFix :: String -> String
dirtyFix "" = ""
dirtyFix (s:ss) | s == '.'  = ' ' : dirtyFix ss
                | otherwise = s : dirtyFix ss

splitFractionWrapper1 :: [Integer] -> Integer
splitFractionWrapper1 xs | myLen xs == 2 = head xs
                         | otherwise     = error "The string was malformed"

splitFractionWrapper2 :: [Integer] -> Integer
splitFractionWrapper2 xs | myLen xs == 2 = head $ tail xs
                         | otherwise     = error "The string was malformed"


-- Chapter 5 - Trees
-- Okay this is not efficient. Moving to different material.
