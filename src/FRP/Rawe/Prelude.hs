{-# LANGUAGE OverloadedStrings #-}


module FRP.Rawe.Prelude (
    Char, String,

    Bool(..), true, false, bool, ite,
    (&&), (||), not,

    Maybe(..), nothing, just, maybe,

    Int, Integer, Float, Double, Rational,
    ($), const, flip,

    curry, uncurry,

    Num(..),
    BEq(..), BFunctor(..), BMonad(..),

    error, undefined,

    nil, (~:), list, map, (++), head, tail, null, length, foldl, foldr, and, or, all, concat, lookup, zip, zipWith,

) where


import qualified Prelude as P
import Prelude (Bool(..), Char, String, Maybe(..), Int, Integer, Float, Double, Rational, ($), Num(..), const, flip)

import Control.Category
import Control.Category.Cartesian

import Text.JSON hiding (toJSString, fromJSString)
import qualified Text.JSON as J

import FRP.Rawe
import FRP.Rawe.Internal



instance BEq Int where (==) = primOp2 (P.==) "eq"


instance BEq Char where (==) = primOp2 (P.==) "eq"


instance BFunctor ((,) a) where
        fmap f x = (fst . x) &&& f (snd . x)



true :: Bhv Bool
true = primOp0 True "true"

false :: Bhv Bool
false = primOp0 False "false"

bool :: Bhv a -> Bhv a -> Bhv Bool -> Bhv a
bool = primOp3 (\t f b -> if b then t else f) "bool"

ite :: Bhv Bool -> Bhv a -> Bhv a -> Bhv a
ite c t f = bool t f c


(&&) :: Bhv Bool -> Bhv Bool -> Bhv Bool
x && y = bool y x x

(||) :: Bhv Bool -> Bhv Bool -> Bhv Bool
x || y = bool x y x

not :: Bhv Bool -> Bhv Bool
not = bool false true


nothing :: Bhv (Maybe a)
nothing = primOp0 Nothing "nothing"

just :: Bhv a -> Bhv (Maybe a)
just = primOp1 Just "just"

maybe :: Bhv b -> (Bhv a -> Bhv b) -> Bhv (Maybe a) -> Bhv b
maybe = primOp3 P.maybe "maybe"

instance BEq a => BEq (Maybe a) where
    x == y = maybe (maybe true (const false) y) (\xv -> maybe false (xv ==) y) x

instance BFunctor Maybe where
    fmap f = maybe nothing (just . f)

instance BMonad Maybe where
    return = just
    x >>= f = maybe nothing f x
    join = maybe nothing id

{-
fromJust :: Bhv (Maybe b) -> Bhv b
fromJust = maybe (error "fromJust: Nothing") id
-}

{-
data Either a b
= Left a 
| Right b 
either :: (a -> c) -> (b -> c) -> Either a b -> c
data Ordering 
= LT 
| EQ 
| GT 
data Char 
type String = [Char]
fst :: (a, b) -> a
snd :: (a, b) -> b
-}

curry :: (Bhv (a, b) -> c) -> Bhv a -> Bhv b -> c
curry = b_curry

uncurry :: (Bhv a -> Bhv b -> c) -> Bhv (a, b) -> c
uncurry = b_uncurry

class BEq a where
    (==) :: Bhv a -> Bhv a -> Bhv Bool
    x == y = not $ x /= y
    (/=) :: Bhv a -> Bhv a -> Bhv Bool
    x /= y = not $ x == y

{-
class Eq a => Ord a where
compare :: a -> a -> Ordering
(<) :: a -> a -> Bool
(>=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(<=) :: a -> a -> Bool
max :: a -> a -> a
min :: a -> a -> a
class Enum a where
succ :: a -> a
pred :: a -> a
toEnum :: Int -> a
fromEnum :: a -> Int
enumFrom :: a -> [a]
enumFromThen :: a -> a -> [a]
enumFromTo :: a -> a -> [a]
enumFromThenTo :: a -> a -> a -> [a]
class Bounded a where
minBound :: a
maxBound :: a
data Int 
data Integer 
data Float 
data Double 
type Rational = Ratio Integer
class (Eq a, Show a) => Num a where
(+) :: a -> a -> a
(*) :: a -> a -> a
(-) :: a -> a -> a
negate :: a -> a
abs :: a -> a
signum :: a -> a
fromInteger :: Integer -> a
class (Num a, Ord a) => Real a where
toRational :: a -> Rational
class (Real a, Enum a) => Integral a where
quot :: a -> a -> a
rem :: a -> a -> a
div :: a -> a -> a
mod :: a -> a -> a
quotRem :: a -> a -> (a, a)
divMod :: a -> a -> (a, a)
toInteger :: a -> Integer
class Num a => Fractional a where
(/) :: a -> a -> a
recip :: a -> a
fromRational :: Rational -> a
class Fractional a => Floating a where
pi :: a
exp :: a -> a
sqrt :: a -> a
log :: a -> a
(**) :: a -> a -> a
logBase :: a -> a -> a
sin :: a -> a
tan :: a -> a
cos :: a -> a
asin :: a -> a
atan :: a -> a
acos :: a -> a
sinh :: a -> a
tanh :: a -> a
cosh :: a -> a
asinh :: a -> a
atanh :: a -> a
acosh :: a -> a
class (Real a, Fractional a) => RealFrac a where
properFraction :: Integral b => a -> (b, a)
truncate :: Integral b => a -> b
round :: Integral b => a -> b
ceiling :: Integral b => a -> b
floor :: Integral b => a -> b
class (RealFrac a, Floating a) => RealFloat a where
floatRadix :: a -> Integer
floatDigits :: a -> Int
floatRange :: a -> (Int, Int)
decodeFloat :: a -> (Integer, Int)
encodeFloat :: Integer -> Int -> a
exponent :: a -> Int
significand :: a -> a
scaleFloat :: Int -> a -> a
isNaN :: a -> Bool
isInfinite :: a -> Bool
isDenormalized :: a -> Bool
isNegativeZero :: a -> Bool
isIEEE :: a -> Bool
atan2 :: a -> a -> a
subtract :: Num a => a -> a -> a
even :: Integral a => a -> Bool
odd :: Integral a => a -> Bool
gcd :: Integral a => a -> a -> a
lcm :: Integral a => a -> a -> a
(^) :: (Num a, Integral b) => a -> b -> a
(^^) :: (Fractional a, Integral b) => a -> b -> a
fromIntegral :: (Integral a, Num b) => a -> b
realToFrac :: (Real a, Fractional b) => a -> b
-}

class BFunctor m => BMonad m where
        return :: Bhv a -> Bhv (m a)

        (>>=) :: Bhv (m a) -> (Bhv a -> Bhv (m b)) -> Bhv (m b)
        x >>= f = join $ fmap f x

        join :: Bhv (m (m a)) -> Bhv (m a)
        join = (>>= id)

class BFunctor f where
    fmap :: (Bhv a -> Bhv b) -> (Bhv (f a)) -> (Bhv (f b))

{-
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
sequence :: Monad m => [m a] -> m [a]
sequence_ :: Monad m => [m a] -> m ()
(=<<) :: Monad m => (a -> m b) -> m a -> m b
id :: a -> a
const :: a -> b -> a
(.) :: (b -> c) -> (a -> b) -> a -> c
flip :: (a -> b -> c) -> b -> a -> c
($) :: (a -> b) -> a -> b
until :: (a -> Bool) -> (a -> a) -> a -> a
asTypeOf :: a -> a -> a
-}

error' :: Bhv JSString -> Bhv a
error' = primOp1 (P.error . J.fromJSString) "error"

error :: Bhv String -> Bhv a
error = error' . toJSString

undefined = error "undefined"

{-
seq :: a -> b -> b
($!) :: (a -> b) -> a -> b
-}

nil :: Bhv [a]
nil = primOp0 [] "nil"

(~:) :: Bhv a -> Bhv [a] -> Bhv [a]
(~:) = primOp2 (:) "cons"
infixr 5 ~:

list :: Bhv b -> (Bhv a -> Bhv [a] -> Bhv b) -> Bhv [a] -> Bhv b
list = primOp3 (\n c l -> case l of [] -> n; (x:xs) -> c x xs) "list"

instance BEq a => BEq [a] where
        (==) = bfix $ (\z -> \xs ys -> list (null ys) (\x xs' -> list false (\y ys' -> (x == y) && z xs' ys') ys) xs)

instance BFunctor [] where
        fmap = map

instance BMonad [] where
        return = (~:nil)
        join = concat

map :: (Bhv a -> Bhv b) -> Bhv [a] -> Bhv [b]
map f = bfix (\m -> list nil (\x xs -> f x ~: m xs))

(++) :: Bhv [a] -> Bhv [a] -> Bhv [a]
xs ++ ys = bfix (\f -> list ys (\x xs' -> x ~: f xs')) xs

--filter :: (a -> Bool) -> [a] -> [a]

head :: Bhv [a] -> Bhv a
head = list (error "Rawe.head") (\x _ -> x)

--last :: [a] -> a

tail :: Bhv [a] -> Bhv [a]
tail = list (error "Rawe.tail") (\_ xs -> xs)

--init :: [a] -> [a]

null :: Bhv [a] -> Bhv Bool
null = list true (\_ _ -> false)

length :: Bhv [b] -> Bhv Int
length = bfix (\f -> list 0 (\_ xs -> 1 + f xs))

{-
(!!) :: [a] -> Int -> a
reverse :: [a] -> [a]
-}

foldl :: (Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv [b] -> Bhv a
foldl f = bfix (\fld -> \z -> list z (\x xs -> fld (f z x) xs))

--foldl1 :: (a -> a -> a) -> [a] -> a

foldr :: (Bhv a -> Bhv b -> Bhv b) -> Bhv b -> Bhv [a] -> Bhv b
foldr f z = bfix (\fld -> list z (\x xs -> x `f` fld xs))

--foldr1 :: (a -> a -> a) -> [a] -> a

and :: Bhv [Bool] -> Bhv Bool
and = foldr (&&) true


or :: Bhv [Bool] -> Bhv Bool
or = foldr (||) false

--any :: (a -> Bool) -> [a] -> Bool

all :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv Bool
all f = and . map f

{-
sum :: Num a => [a] -> a
product :: Num a => [a] -> a
-}

concat :: Bhv [[a]] -> Bhv [a]
concat = foldr (++) nil

{-
concatMap :: (a -> [b]) -> [a] -> [b]
maximum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]
iterate :: (a -> a) -> a -> [a]
repeat :: a -> [a]
replicate :: Int -> a -> [a]
cycle :: [a] -> [a]
take :: Int -> [a] -> [a]
drop :: Int -> [a] -> [a]
splitAt :: Int -> [a] -> ([a], [a])
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
span :: (a -> Bool) -> [a] -> ([a], [a])
break :: (a -> Bool) -> [a] -> ([a], [a])
elem :: Eq a => a -> [a] -> Bool
notElem :: Eq a => a -> [a] -> Bool
-}

lookup :: (BEq a) => Bhv a -> Bhv [(a,b)] -> Bhv (Maybe b)
lookup k = bfix $ \f -> list nothing $ \x xs -> flip uncurry x $ \k' v -> ite (k'==k) (just v) (f xs)

zip :: Bhv [a] -> Bhv [b] -> Bhv [(a, b)]
zip = zipWith (&&&)

--zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

zipWith :: (Bhv a -> Bhv b -> Bhv c) -> Bhv [a] -> Bhv [b] -> Bhv [c]
zipWith f = bfix $ (\z -> \xs ys -> list nil (\x xs' -> list nil (\y ys' -> f x y ~: z xs' ys') ys) xs)

{-
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
lines :: String -> [String]
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String
type ShowS = String -> String
class Show a where
showsPrec :: Int -> a -> ShowS
show :: a -> String
showList :: [a] -> ShowS
shows :: Show a => a -> ShowS
showChar :: Char -> ShowS
showString :: String -> ShowS
showParen :: Bool -> ShowS -> ShowS
type ReadS a = String -> [(a, String)]
class Read a where
readsPrec :: Int -> ReadS a
readList :: ReadS [a]
reads :: Read a => ReadS a
readParen :: Bool -> ReadS a -> ReadS a
read :: Read a => String -> a
lex :: ReadS String
data IO a
putChar :: Char -> IO ()
putStr :: String -> IO ()
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
getChar :: IO Char
getLine :: IO String
getContents :: IO String
interact :: (String -> String) -> IO ()
type FilePath = String
readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()
readIO :: Read a => String -> IO a
readLn :: Read a => IO a
type IOError = IOException
ioError :: IOError -> IO a
userError :: String -> IOError
catch :: IO a -> (IOError -> IO a) -> IO a
-}
