module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O     = S O   -- True
isZero (S _) = O     -- False


-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O    =  O
pred (S n) = n

-- Output: O means False, S O means True
even :: Nat -> Nat
even O         = S    O
even (S O)     =      O
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = O
odd (S O) = S O
odd (S (S n)) = odd n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O = n
monus O _ = O
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
(*) :: Nat -> Nat -> Nat
(*) n O = O
(*) n (S m) = n + (n * m)

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) n O = S O
(^) n (S m) = n * (n ^ m)

-- decide: infix? ? ^
infixr 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
O / O  = undefined
S n / O = undefined
O / S m = O
S m / S n = case n -* m of
                    O -> S O
                    n -> S n / S m




-- remainder
(%) :: Nat -> Nat -> Nat
_ % O = undefined
x % y = x -* (y * (x/y))

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) O _ = undefined -- 0 does not divide anything
(|||) _ O = S O -- every number divides 0
(|||) x y = (x % y) `isZero` -- if remainder is 0, then x divides y

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff O m = m
absDiff n O = n
absDiff (S n) (S m) = absDiff n m
infixl 6 |-| -- same precendence as + and -*

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = (S n) * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo (S O) _ = undefined
lo _ O = undefined
lo b (S O) = O
lo b a = case isZero (a *- b) of
           S O -> O
            O -> S (lo b (a / b))


