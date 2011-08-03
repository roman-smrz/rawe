{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Prelude.hs: reimplementation of most of the Haskell standard prelude
-- as part of rawe - ReActive Web Framework

-----------------------------------------------------------------------------
-- |
-- Maintainer  : Roman Smrž <roman.smrz@seznam.cz>
-- Stability   : experimental
--
-- In this module is reimplemented most of Haskell Standard Prelude and the
-- code is partially copied directly from the Haskell Report, see
-- <http://www.haskell.org/onlinereport/standard-prelude.html>.
--
-- Not all the functionality is provided, though, missing are:
--
--  * All the IO-related functions.
--
--  * Enum, Bouded classes.
--
--  * Text-manipulation definitions, including classes Read, Show and functions
--  lines or words.
--
--  * Of the numerict types, supported are only Int and Float and we use the
--  Num class to use numeric literals and provide some operators. Other numeric
--  classes are not provided; div and (/) are defined specifically for Int and
--  Float, respectively.
--  
--  * seq and ($!).
--
--  * Zipping and unzipping functions for three list / 3-tuples.


module FRP.Rawe.Prelude (

    -- * Standard types, classes, instances and related functions

    Bool(False, True), true, false, bool, ite,
    Maybe(Nothing, Just), nothing, just, maybe,
    Either(Left, Right), left, right, either,
    Ordering(LT, EQ, GT), lt, eq, gt, ordering,
    Char, String, Int, Float, Double,
    list, nil, (~:),

    BEq((==), (/=)),
    BOrd(compare, (<), (<=), (>=), (>), max, min),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    div, (/),
    BMonad((>>=), (>>), join, return, fail),
    BFunctor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    (&&), (||), not,
    fst, snd, (&&&), curry, uncurry, id, const, (.), flip, ($),
    asTypeOf, error, undefined,

    -- * List operations
    map, (++), filter, concat, concatMap,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum,
    zip, zipWith, unzip,


    -- * Rawe-specific declarations

    -- ** Generalized currying and fixpoint operator

    BhvCurrying(..),
    bjoin,
    fix, BhvFix(..),

    -- ** Events
    Event,
    evfold, evmerge, evjoin, evguard, 
    switcher,

) where

import qualified Prelude as P
import Prelude(
        Bool(False, True),
        Maybe(Nothing, Just),
        Either(Left, Right),
        Ordering(LT, EQ, GT),
        Char, String, Int, Float, Double,

        Num((+), (-), (*), negate, abs, signum, fromInteger),

        id, const, (.), flip, ($),
        asTypeOf,
    )

import qualified Control.Category.Cartesian as C
import Control.Category.Cartesian ((&&&))

import qualified Text.JSON as J
import Text.JSON (JSValue, JSString)

import FRP.Rawe
import FRP.Rawe.Internal

infixl 7  /
infix  4  ==, /=, <, <=, >=, >
infixr 3  &&
infixr 2  ||
infixl 1  >>, >>=
infixr 1  =<<

infixr 5  ~:
infixl 9  !!
infixr 5  ++
infix  4  `elem`, `notElem`

-- Standard types, classes, instances and related functions

-- Equality and Ordered classes


class  BEq a  where
    (==), (/=) :: Bhv a -> Bhv a -> Bhv Bool

        -- Minimal complete definition:
        --      (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)


class  (BEq a) => BOrd a  where
    compare              :: Bhv a -> Bhv a -> Bhv Ordering
    (<), (<=), (>=), (>) :: Bhv a -> Bhv a -> Bhv Bool
    max, min             :: Bhv a -> Bhv a -> Bhv a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y = ite (x == y) eq (
                  ite (x <= y) lt gt)

    x <= y           =  compare x y /= gt
    x <  y           =  compare x y == lt
    x >= y           =  compare x y /= lt
    x >  y           =  compare x y == gt

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y = ite (x <= y) y x
    min x y = ite (x <= y) x y



-- Monadic classes


class  BFunctor f  where
    fmap   :: (Bhv a -> Bhv b) -> Bhv (f a) -> Bhv (f b)


class  BFunctor m => BMonad m  where
    (>>=)  :: Bhv (m a) -> (Bhv a -> Bhv (m b)) -> Bhv (m b)
    (>>)   :: Bhv (m a) -> Bhv (m b) -> Bhv (m b)
    return :: Bhv a -> Bhv (m a)
    fail   :: Bhv String -> Bhv (m a)
    join   :: Bhv (m (m a)) -> Bhv (m a)

    m >> k  =  m >>= \_ -> k
    fail s  = error s

    x >>= f = join $ fmap f x
    join = (>>= id)


sequence       :: BMonad m => Bhv [m a] -> Bhv (m [a])
sequence       =  foldr mcons (return nil)
                    where mcons p q = p >>= \x -> q >>= \y -> return (x~:y)


sequence_      :: BMonad m => Bhv [m a] -> Bhv (m ())
sequence_      =  foldr (>>) (return unit)

-- The xxxM functions take list arguments, but lift the function or
-- list element to a monad type

mapM             :: BMonad m => (Bhv a -> Bhv (m b)) -> Bhv [a] -> Bhv (m [b])
mapM f as        =  sequence (map f as)


mapM_            :: BMonad m => (Bhv a -> Bhv (m b)) -> Bhv [a] -> Bhv (m ())
mapM_ f as       =  sequence_ (map f as)


(=<<)            :: BMonad m => (Bhv a -> Bhv (m b)) -> Bhv (m a) -> Bhv (m b)
f =<< x          =  x >>= f


-- Trivial type

unit :: Bhv ()
unit = cb ()

instance BEq () where _ == _ = true
instance BOrd () where _ <= _ = true




-- Boolean type

true :: Bhv Bool
true = primOp0 True "true"

false :: Bhv Bool
false = primOp0 False "false"

bool :: Bhv a -> Bhv a -> Bhv Bool -> Bhv a
bool = primOp3 (\t f b -> if b then t else f) "bool"

ite :: Bhv Bool -> Bhv a -> Bhv a -> Bhv a
ite c t f = bool t f c


-- Boolean functions


(&&), (||) :: Bhv Bool -> Bhv Bool -> Bhv Bool
x && y = bool y x x
x || y = bool x y x


not :: Bhv Bool -> Bhv Bool
not = bool false true


-- Character type


instance  BEq Char  where (==) = primOp2 (P.==) "cmp_eq"
instance  BOrd Char  where (<=) = primOp2 (P.<=) "cmp_le"



-- Maybe type


nothing :: Bhv (Maybe a)
nothing = primOp0 Nothing "nothing"

just :: Bhv a -> Bhv (Maybe a)
just = primOp1 Just "just"

maybe :: Bhv b -> (Bhv a -> Bhv b) -> Bhv (Maybe a) -> Bhv b
maybe = primOp3 P.maybe "maybe"


instance BEq a => BEq (Maybe a) where
    x == y = maybe (maybe true (const false) y) (\xv -> maybe false (xv ==) y) x

instance BOrd a => BOrd (Maybe a) where
    compare x y = maybe (maybe eq (const lt) y) (\xv -> maybe gt (compare xv) y) x

instance BFunctor Maybe where
    fmap f = maybe nothing (just . f)

instance BMonad Maybe where
    return = just
    x >>= f = maybe nothing f x

-- Either type

left :: Bhv a -> Bhv (Either a b)
left = primOp1 Left "left"

right :: Bhv b -> Bhv (Either a b)
right = primOp1 Right "right"

either :: (Bhv a -> Bhv c) -> (Bhv b -> Bhv c) -> Bhv (Either a b) -> Bhv c
either = primOp3 P.either "either"

instance (BEq a, BEq b) => BEq (Either a b) where
    x == y  =  either
                (\xv -> either (xv==) (const false) y)
                (\xv -> either (const false) (xv==) y)
                x

instance (BOrd a, BOrd b) => BOrd (Either a b) where
    compare x y = either
                    (\xv -> either (compare xv) (const lt) y)
                    (\xv -> either (const gt) (compare xv) y)
                    x

-- Ordering type

ordering :: Bhv a -> Bhv a -> Bhv a -> Bhv Ordering -> Bhv a
ordering = primOp4 (\l e g o -> case o of LT -> l; EQ -> e; GT -> g) "ordering"

lt :: Bhv Ordering
lt = primOp0 LT "lt"

eq :: Bhv Ordering
eq = primOp0 EQ "eq"

gt :: Bhv Ordering
gt = primOp0 GT "gt"

instance BEq Ordering where
    (==) = primOp2 (P.==) "cmp_eq"


-- Standard numeric types.


instance  BEq Int  where (==) = primOp2 (P.==) "cmp_eq"
instance  BOrd Int  where (<=) = primOp2 (P.<=) "cmp_le"

instance P.Eq (Bhv a) where
    _ == _ = P.error "Eq instance for behaviours is required for Num, but is meaningless"
instance P.Show (Bhv a) where
    show _ = P.error "Show instance for behaviours is required for Num, but is meaningless"

instance Num (Bhv Int) where
    fromInteger = prim . BhvConst . fromInteger
    (+) = primOp2 (+) "arit_plus"
    (-) = primOp2 (-) "arit_minus"
    (*) = primOp2 (*) "arit_times"
    negate = primOp1 negate "arit_neg"
    abs = primOp1 abs "arit_abs"
    signum x = ite (x > 0) 1 (ite (x < 0) (-1) 0)

div :: Bhv Int -> Bhv Int -> Bhv Int
div = primOp2 (P.div) "idiv"

instance  BEq Float  where (==) = primOp2 (P.==) "cmp_eq"
instance  BOrd Float  where (<=) = primOp2 (P.<=) "cmp_le"

instance Num (Bhv Float) where
    fromInteger = prim . BhvConst . fromInteger
    (+) = primOp2 (+) "arit_plus"
    (-) = primOp2 (-) "arit_minus"
    (*) = primOp2 (*) "arit_times"
    negate = primOp1 negate "arit_neg"
    abs = primOp1 abs "arit_abs"
    signum x = ite (x > 0) 1 (ite (x < 0) (-1) 0)

(/) :: Bhv Float -> Bhv Float -> Bhv Float
(/) = primOp2 (P./) "div"


-- Lists


nil :: Bhv [a]
nil = primOp0 [] "nil"

(~:) :: Bhv a -> Bhv [a] -> Bhv [a]
(~:) = primOp2 (:) "cons"

list :: Bhv b -> (Bhv a -> Bhv [a] -> Bhv b) -> Bhv [a] -> Bhv b
list = primOp3 (\n c l -> case l of [] -> n; (x:xs) -> c x xs) "list"


instance BEq a => BEq [a] where
    (==) = bfix $ \z xs ys -> list (null ys)
                                   (\x xs' -> list false
                                                   (\y ys' -> (x == y) && z xs' ys')
                                                   ys
                                   )
                                   xs

instance BOrd a => BOrd [a] where
    (<=) = bfix $ \f xs ys -> list true
                                   (\x xs' -> list false
                                                   (\y ys' -> (x < y) || (x == y && f xs' ys'))
                                                   ys
                                   )
                                   xs

instance BFunctor [] where
    fmap = map


instance  BMonad []  where
    m >>= k          = concat (map k m)
    return x         = x ~: nil
    fail s           = nil

-- Tuples

fst :: Bhv (a,b) -> Bhv a
fst = bhvToHask C.fst

snd :: Bhv (a,b) -> Bhv b
snd = bhvToHask C.snd

instance (BEq a, BEq b) => BEq (a,b) where
    x == y  =  fst x == fst y && snd x == snd y

instance (BOrd a, BOrd b) => BOrd (a,b) where
    x <= y  =  (fst x < fst y) || (fst x == fst y && snd x <= snd y)

instance BFunctor ((,) a) where
    fmap f x = (fst x) &&& f (snd x)

-- curry converts an uncurried function to a curried function;
-- uncurry converts a curried function to a function on pairs.

curry            :: (Bhv (a, b) -> c) -> Bhv a -> Bhv b -> c
curry f x y      =  f (x &&& y)


uncurry          :: (Bhv a -> Bhv b -> c) -> (Bhv (a, b) -> c)
uncurry f p      =  f (fst p) (snd p)



-- error stops execution and displays an error message

error'           :: Bhv JSString -> Bhv a
error'           = primOp1 (P.error . J.fromJSString) "error"

error            :: Bhv String -> Bhv a
error            = error' . toJSString

-- It is expected that compilers will recognize this and insert error
-- messages that are more appropriate to the context in which undefined
-- appears.


undefined        :: Bhv a
undefined        =  error "Rawe.Prelude.undefined"


-- Map and append

map :: (Bhv a -> Bhv b) -> Bhv [a] -> Bhv [b]
map f = bfix (\m -> list nil (\x xs -> f x ~: m xs))


(++) :: Bhv [a] -> Bhv [a] -> Bhv [a]
xs ++ ys = bfix (\f -> list ys (\x xs' -> x ~: f xs')) xs


filter :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv [a]
filter p = bfix (\f -> list nil (\x xs -> ite (p x) (x ~: f xs) (f xs)))


concat :: Bhv [[a]] -> Bhv [a]
concat xss = foldr (++) nil xss


concatMap :: (Bhv a -> Bhv [b]) -> Bhv [a] -> Bhv [b]
concatMap f = concat . map f

-- head and tail extract the first element and remaining elements,
-- respectively, of a list, which must be non-empty.  last and init
-- are the dual functions working from the end of a finite list,
-- rather than the beginning.


head             :: Bhv [a] -> Bhv a
head             =  list (error "Rawe.Prelude.head: empty list") const

tail             :: Bhv [a] -> Bhv [a]
tail             =  list (error "Rawe.Prelude.tail: empty list") (flip const)



last :: Bhv [a] -> Bhv a
last = bfix $ \lst -> list (error "Rawe.Prelude.last: empty list")
                            (\x xs -> list x (\_ _ -> lst xs) xs)


init :: Bhv [a] -> Bhv [a]
init = bfix $ \int -> list (error "Prelude.init: empty list")
                            (\x xs -> list nil (\_ _ -> x ~: int xs) xs)


null :: Bhv [a] -> Bhv Bool
null = list true (\_ _ -> false)


-- length returns the length of a finite list as an Int.

length :: Bhv [a] -> Bhv Int
length = bfix $ \len -> list 0 (\_ xs -> 1 + len xs)

-- List index (subscript) operator, 0-origin

(!!)                :: Bhv [a] -> Bhv Int -> Bhv a
xs !! n = ite (n < 0) (error "Rawe.Prelude.!!: negative index") (
                bfix (\f n' -> list (error "Rawe.Prelude.!!: index too large")
                        (\x xs -> ite (n' == 0) x (f (n'-1) xs))
                ) n xs
            )

-- foldl, applied to a binary operator, a starting value (typically the
-- left-identity of the operator), and a list, reduces the list using
-- the binary operator, from left to right:
--  foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
-- foldl1 is a variant that has no starting value argument, and  thus must
-- be applied to non-empty lists.  scanl is similar to foldl, but returns
-- a list of successive reduced values from the left:
--      scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
-- Note that  last (scanl f z xs) == foldl f z xs.
-- scanl1 is similar, again without the starting element:
--      scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]



foldl :: (Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv [b] -> Bhv a
foldl f = bfix (\fld -> \z -> list z (\x xs -> fld (f z x) xs))


foldl1  :: (Bhv a -> Bhv a -> Bhv a) -> Bhv [a] -> Bhv a
foldl1 f = list (error "Rawe.Prelude.foldl1: empty list") (foldl f)


scanl :: (Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv [b] -> Bhv [a]
scanl f = bfix (\scn -> \q xs -> q ~: list nil (\x xs -> scn (f q x) xs) xs)


scanl1 :: (Bhv a -> Bhv a -> Bhv a) -> Bhv [a] -> Bhv [a]
scanl1 f = list nil (scanl f)


-- foldr, foldr1, scanr, and scanr1 are the right-to-left duals of the
-- above functions.


foldr :: (Bhv a -> Bhv b -> Bhv b) -> Bhv b -> Bhv [a] -> Bhv b
foldr f z = bfix (\fld -> list z (\x xs -> x `f` fld xs))


foldr1 :: (Bhv a -> Bhv a -> Bhv a) -> Bhv [a] -> Bhv a
foldr1 f = bfix $ \fld -> list (error "Rawe.Prelude.foldr1: empty list") $
                               \x xs -> list x (\_ _ -> f x (fld xs)) xs


scanr             :: (Bhv a -> Bhv b -> Bhv b) -> Bhv b -> Bhv [a] -> Bhv [b]
scanr f q0 = bfix $ \scn -> list (q0 ~: nil) (\x xs -> let qs = scn xs in f x (head qs) ~: qs)


scanr1          :: (Bhv a -> Bhv a -> Bhv a) -> Bhv [a] -> Bhv [a]
scanr1 f = bfix $ \scn -> list nil (\x xs -> list (x~:nil) (\_ _ -> let qs = scn xs in f x (head qs) ~: qs) xs)

-- iterate f x returns an infinite list of repeated applications of f to x:
-- iterate f x == [x, f x, f (f x), ...]

iterate :: (Bhv a -> Bhv a) -> Bhv a -> Bhv [a]
iterate f = bfix $ \itr x -> x ~: itr (f x)

-- repeat x is an infinite list, with x the value of every element.

repeat :: Bhv a -> Bhv [a]
repeat x = bfix $ (x ~:)

-- replicate n x is a list of length n with x the value of every element

replicate        :: Bhv Int -> Bhv a -> Bhv [a]
replicate n x    =  take n (repeat x)

-- cycle ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.


cycle :: Bhv [a] -> Bhv [a]
cycle = concat . repeat

-- take n, applied to a list xs, returns the prefix of xs of length n,
-- or xs itself if n > length xs.  drop n xs returns the suffix of xs
-- after the first n elements, or [] if n > length xs.  splitAt n xs
-- is equivalent to (take n xs, drop n xs).

take :: Bhv Int -> Bhv [a] -> Bhv [a]
take = bfix $ \tk n xs -> ite (n <= 0) nil $ list nil (\x xs -> x ~: tk (n-1) xs) xs

drop :: Bhv Int -> Bhv [a] -> Bhv [a]
drop = bfix $ \dr n xs -> ite (n <= 0) xs $ list nil (\_ -> dr (n-1)) xs


splitAt                  :: Bhv Int -> Bhv [a] -> Bhv ([a],[a])
splitAt n xs             =  take n xs &&& drop n xs

-- takeWhile, applied to a predicate p and a list xs, returns the longest
-- prefix (possibly empty) of xs of elements that satisfy p.  dropWhile p xs
-- returns the remaining suffix.  span p xs is equivalent to
-- (takeWhile p xs, dropWhile p xs), while break p uses the negation of p.


takeWhile               :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv [a]
takeWhile p = bfix (\t -> list nil (\x xs -> ite (p x) (x ~: t xs) nil))


dropWhile               :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv [a]
dropWhile p = bfix (\d -> list nil (\x xs -> ite (p x) (d xs) (x ~: xs)))


span, break             :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv ([a],[a])

span p = bfix (\span' -> list (nil &&& nil) (\x xs -> let yz = span' xs
                                                       in ite (p x)
                                                           ((x ~: fst yz) &&& (snd yz))
                                                           (nil &&& (x ~: xs))
                                               )
                )

break p                 =  span (not . p)


-- reverse xs returns the elements of xs in reverse order.  xs must be finite.

reverse          :: Bhv [a] -> Bhv [a]
reverse          =  foldl (flip (~:)) nil

-- and returns the conjunction of a Boolean list.  For the result to be
-- True, the list must be finite; False, however, results from a False
-- value at a finite index of a finite or infinite list.  or is the
-- disjunctive dual of and.

and, or          :: Bhv [Bool] -> Bhv Bool
and              =  foldr (&&) true
or               =  foldr (||) false

-- Applied to a predicate and a list, any determines if any element
-- of the list satisfies the predicate.  Similarly, for all.

any, all         :: (Bhv a -> Bhv Bool) -> Bhv [a] -> Bhv Bool
any p            =  or . map p
all p            =  and . map p

-- elem is the list membership predicate, usually written in infix form,
-- e.g., x `elem` xs.  notElem is the negation.

elem, notElem    :: (BEq a) => Bhv a -> Bhv [a] -> Bhv Bool
elem x           =  any (== x)
notElem x        =  all (/= x)

-- lookup key assocs looks up a key in an association list.

lookup :: (BEq a) => Bhv a -> Bhv [(a,b)] -> Bhv (Maybe b)
lookup k = bfix $ \f -> list nothing $ \x xs -> flip uncurry x $ \k' v -> ite (k'==k) (just v) (f xs)

-- sum and product compute the sum or product of a finite list of numbers.

sum, product     :: (Num (Bhv a)) => Bhv [a] -> Bhv a
sum              =  foldl (+) 0
product          =  foldl (*) 1

-- maximum and minimum return the maximum or minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.

maximum, minimum :: (BOrd a) => Bhv [a] -> Bhv a
maximum          =  foldl1 max
minimum          =  foldl1 min

-- zip takes two lists and returns a list of corresponding pairs.  If one
-- input list is short, excess elements of the longer list are discarded.

zip :: Bhv [a] -> Bhv [b] -> Bhv [(a, b)]
zip = zipWith (&&&)


-- The zipWith family generalises the zip family by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, zipWith (+) is applied to two lists to produce the list
-- of corresponding sums.


zipWith :: (Bhv a -> Bhv b -> Bhv c) -> Bhv [a] -> Bhv [b] -> Bhv [c]
zipWith f = bfix $ (\z -> \xs ys -> list nil (\x xs' -> list nil (\y ys' -> f x y ~: z xs' ys') ys) xs)



-- unzip transforms a list of pairs into a pair of lists.


unzip            :: Bhv [(a,b)] -> Bhv ([a],[b])
unzip            =  foldr (\x xs -> (fst x ~: fst xs) &&& (snd x ~: snd xs)) (nil &&& nil)



-- Rawe-specific declarations

-- | Class allowing currying and uncurrying of functions of arbitrary arity.

class BhvCurrying a ps r | a -> ps r where
        -- | uncurryAll has the type
        -- (Bhv a1 -> Bhv a2 -> ... -> Bhv an -> Bhv b) ->
        -- (Bhv (a1, (a2, ( ... (an-1, an) ... ))) -> Bhv b)
        uncurryAll :: a -> Bhv ps -> r

        -- | curryAll has the type
        -- (Bhv (a1, (a2, ( ... (an-1, an) ... ))) -> Bhv b) ->
        -- (Bhv a1 -> Bhv a2 -> ... -> Bhv an -> Bhv b)
        curryAll :: (Bhv ps -> r) -> a

instance BhvCurrying (Bhv a -> Bhv b) a (Bhv b) where
        uncurryAll = id
        curryAll = id

instance (BhvCurrying (Bhv b -> r) ps r') =>
        BhvCurrying (Bhv a -> Bhv b -> r) (a, ps) r' where

        uncurryAll f = uncurry (\x -> uncurryAll (f x))
        curryAll f = \x -> curryAll $ (curry f) x


-- | Somwhat generalized version of joining two layers of Bhv

bjoin :: Bhv (BhvFun a b) -> BhvFun a b
bjoin = prim . BhvModifier (unsafeBfEval . ($void)) "bjoin"

-- | Fixpoint operator for single behaviour

fix :: (Bhv a -> Bhv a) -> Bhv a
fix = primOp1 (\f -> let x = f x in x) "fix"


-- | Class generalizing the fixpoint for function of arbitrary arity. It allows
-- to write recursive functions like
--
-- > foldl :: (Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Bhv [b] -> Bhv a
-- > foldl f = bfix (\fld -> \z -> list z (\x xs -> fld (f z x) xs))

class BhvFix a where
        bfix :: (a -> a) -> a

instance BhvFix (Bhv a) where
        bfix = fix

instance (BhvFix b, BhvCurrying (Bhv a -> b) ps (Bhv d)) => BhvFix (Bhv a -> b) where
        bfix f = curryAll $ bhvToHask $ bjoin $ fix
            (cb . haskToBhv . uncurryAll . f . curryAll . bhvToHask . bjoin)



instance P.Eq Time where _ == _ = True 
instance P.Ord Time where _ <= _ = True 
instance BEq Time  where (==) = primOp2 (P.==) "cmp_eq"
instance BOrd Time  where (<=) = primOp2 (P.<=) "cmp_le"


instance BFunctor Timed where
    fmap f = timed notYet (\t x -> onTime t (f x))

-- | The most general interface to events. Does a left fold over all the values
-- of given event up to the current time. For example
--
-- > evfold (const (+)) (0::Bhv Int) $ fmap (const 1) ev
--
-- counts number of occurences of given event (ev)

evfold :: (Bhv Time -> Bhv a -> Bhv b -> Bhv a) -> Bhv a -> Event b -> Bhv a
evfold = timedFold

-- | Merges two events - occurrences of the resulting event are combinations of
-- the occurrences of the two parameters; if two events happen at the same
-- time, the function in the first parameter is used to merge them (since each
-- occurrence of given even has to have unique time).

evmerge :: (Bhv a -> Bhv a -> Bhv a) -> Event a -> Event a -> Event a
evmerge f x y = timed y (\tx vx -> timed x (\ty vy -> ordering y (onTime tx (f vx vy)) x (compare tx ty) ) y) x

-- | Joins two layers of events

evjoin :: Event (Event a) -> Event a
evjoin = bjoin . timed (cb notYet) (const id)

-- | Removes any occurences of an event for which the predicate does not hold.

evguard :: (Bhv a -> Bhv Bool) -> Event a -> Event a
evguard p = evfold (\t x n -> ite (p n) (onTime t n) x) notYet

-- | 'b `switcher` e' behaves initially like 'b' and changes on each occurrence
-- of 'e' to the behaviour provided by it.

switcher :: Bhv a -> Event (Bhv a) -> Bhv a
switcher def = timed def (const bjoin)
