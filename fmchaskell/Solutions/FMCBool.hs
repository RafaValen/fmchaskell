module FMCBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    , String
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show :: Bool -> String
    show False = "False"
    show True = "True"


  

instance Enum Bool where

    toEnum :: Int -> Bool
    toEnum 0 = False
    toEnum _ = True

    fromEnum :: Bool -> Int
    fromEnum False = 0
    fromEnum True = 1

-- conjunction (AND)
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
True || _   = True
_    || True = True
False || False = False


infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
_    /|\ _    = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
_     \|/ _    = False

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True <=/=> False = True
False <=/=> True = True 
_   <=/=> _     = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> b = b

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
b <== False = True
b <== True = b

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
a <=> b = (a ==> b) && (b ==> a)
infixr 1 <=>


