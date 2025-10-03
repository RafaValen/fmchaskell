{-# LANGUAGE GADTs #-}

module FMCNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    , String
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show :: Nat -> String
    show O = "O"
    show (S n) = "S" ++ show n


instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    O == (S _) = False
    (S _) == O = False
    (S n) == (S m) = n == m

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    (S _) <= O = False
    (S n) <= (S m) = n <= m
    
    
    

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S m) (S n) = S (min m n)

    max O (S m)  = (S m)
    max (S m) O = (S m)
    max (S m) (S n) = S (max m n)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O <+> n = n
(S m) <+> n = S (m <+> n)
infixl 6 <+>

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
infixl 6 -*

-- multiplication
times :: Nat -> Nat -> Nat
times O _ = O
times (S m) n = n <+> (m `times` n)
infixl 7 <*>

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow n O = S O
pow n (S m) = n `times` (n `pow` m)
infixr 8 <^>

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient
(</>) :: Nat -> Nat -> Nat
O </> O = undefined
(S n) </> O = undefined
O </> (S m) = O
(S m) </> (S n) = case n -* m of
                    O -> S O
                    k -> S m </> S k
infixl 7 </>


-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = undefined
n <%> m = n -* (m <*> (n </> m))
infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (O, m) = (O, O)
eucdiv (n, O) = undefined
eucdiv (n, m) = (q, r)
    where
        q = n </> m
        r = n <%> m
-- divides
(<|>) :: Nat -> Nat -> Bool
O <|> O = True
O <|> (S _) = True
(S _) <|> O = False
(S n) <|> (S m) = (n -* m) <|> S m
infixl 7 <|>

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m = (n -* m) <+> (m -* n)
infixl 6 |-|

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = (S n) <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = error "lo: base 0"
lo (S O) _ = error "lo: base 1"
lo _ O = error "lo: log of 0"
lo b (S O) = O
lo b a = S (lo b (a </> b))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n
  | n < 0     = error "negative number"
  | n == 0    = O
  | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n



-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

