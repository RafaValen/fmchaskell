{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = error "head: empty list"
head (x : _) = x

tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null (_ : _) = False

length :: Integral i => [a] -> i
length [] = 0
length (_ : xs) = 1 + length xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
(++) []   ys = ys
(++) (x:xs) ys = x : (xs ++ ys)
 
-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
infixl 5 `snoc`

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
minimum [] = error "minimum: empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs)

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum [] = error "maximum: empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)


-- take
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ [] = []
take n (x:xs) = x : take (n-1) xs
-- drop
drop :: Int -> [a] -> [a]
drop n xs | n <= 0 = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x    = x : takeWhile p xs
  | otherwise = []
-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p xs@(x:xs')
  | p x    = dropWhile p xs'

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'
-- init
init :: [a] -> [a]
init [] = error "init: empty list"
init [_] = []
init (x:xs) = x : init xs
-- inits
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]

-- subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = let subs = subsequences xs in subs ++ L.map (x:) subs
-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs
-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

-- and
and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && and bs

-- or
or :: [Bool] -> Bool
or [] = False
or (b:bs) = b || or bs

-- concat
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem y  = any (== y) 


-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) = (y == x) || elem' y xs

-- (!!)
(!!) :: [a] -> Int -> a
(!!) [] _ = error "(!!): index too large"
(!!) (x:_) 0 = x
(!!) (_:xs) n | n < 0     = error "(!!): negative index"
             | otherwise = xs !! (n-1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = xs ++ cycle xs
-- repeat
repeat :: a -> [a]
repeat x = xs where xs = x : xs
-- replicate
replicate :: Int -> a -> [a]
replicate n x | n <= 0    = []
               | otherwise = x : replicate (n-1) x

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = (x == y) && isPrefixOf xs ys
-- isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf needle haystack = reverse needle `isPrefixOf` reverse haystack

-- zip
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [xs] = xs
intercalate sep (xs:xss) = xs ++ sep ++ intercalate sep xss
-- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- splitAt
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs | n <= 0 = ([], xs)
splitAt _ [] = ([], [])
splitAt n (x:xs) = let (ys, zs) = splitAt (n-1) xs in (x:ys, zs)  
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break
break :: (a -> Bool) -> [a] -> ([a],[a])
break _ [] = ([], [])
break p xs@(x:xs')
  | p x       = ([], xs)
  | otherwise = let (ys, zs) = break p xs' in (x:ys, zs)
-- span
span :: (a -> Bool) -> [a] -> ([a],[a])
span _ [] = ([], [])
span p xs@(x:xs')
  | p x       = let (ys, zs) = span p xs' in (x:ys, zs)
  | otherwise = ([], xs)

-- lines
lines :: String -> [String]
lines [] = []
lines xs = let (pre, suf) = break (== '\n') xs
           in pre : case suf of   
                      []      -> []
                      (_:suf') -> lines suf'
-- words
words :: String -> [String]
words [] = []
words xs = let (pre, suf) = break C.isSpace xs
           in pre : case suf of
                      []      -> []
                      (_:suf') -> words suf'
-- unlines
unlines :: [String] -> String
unlines [] = []
unlines (x:xs) = x ++ '\n' : unlines xs
-- unwords
unwords :: [String] -> String
unwords [] = []
unwords [x] = x
unwords (x:xs) = x ++ ' ' : unwords xs

-- transpose
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome xs = let ys = L.map C.toLower (filter C.isAlpha xs) in ys == reverse ys



{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

