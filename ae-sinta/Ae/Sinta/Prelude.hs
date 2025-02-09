{-# LANGUAGE RebindableSyntax #-}

module Ae.Sinta.Prelude where

import Ae.Sinta.TH
    ( makeUnbounds, makeMorphs, makeMorphs2, makeMorphs3 )
import Ae.Sinta.Internal.Types (type (:>>), Eff', S', Ret)
import Ae.Sinta.Internal.Core
    ( Unbound,
      Morph,
      Morph2,
      Lit,
      Morph3,
      lit, Lit, morph3, runMorph3 )

import Prelude ( String, Bool(..), Integer, flip, Int, Rational, (.), Maybe, type (~) )

import Data.Kind (Type)
import Data.Rank1Typeable (ANY2, ANY1)
import Unsafe.Coerce (unsafeCoerce)

import Effectful ( type (:>), Effect, Eff )
import Effectful.Internal.Effect ( type (++) )
import Effectful.TH (makeEffect)

-- * Prelude of Sinta

-- ** Basic operators and related functions

-- *** Categorical

data SintaApply f :: Effect where
    Apply :: forall a b f m.
        m (f (a -> b)) -> m (f a) -> SintaApply f m (f b)

makeEffect ''SintaApply

-- *** Logic

makeMorphs
    [ "not" ]
makeMorphs2
    [ "==", "/=", "&&", "||" ]

infix 4 ==, /=
infixr 3 &&
infixr 2 ||

data SintaIfThenElse f c :: Effect where
    IfThenElse :: forall r c f m.
        m (f c) -> m (f r) -> m (f r) -> SintaIfThenElse f c m (f r)

makeEffect ''SintaIfThenElse

type SintaBool f = 
    [ SintaIfThenElse f Bool
    , Morph "not" f Bool Bool
    , Morph2 "&&" f Bool Bool Bool
    , Morph2 "||" f Bool Bool Bool ]

type SintaEq f a =
    SintaBool f ++
    [ Morph2 "==" f a a Bool
    , Morph2 "/=" f a a Bool ]

true :: (Lit f Bool :> es) => Eff' es f Bool
true = lit True

false :: (Lit f Bool :> es) => Eff' es f Bool
false = lit False

otherwise :: (Lit f Bool :> es) => Eff' es f Bool
otherwise = lit False

-- *** Maybe

type SintaMaybe f =
    '[ Morph3 "maybe" f ANY2 (ANY1 -> ANY2) (Maybe ANY1) ANY2 ]

maybe :: forall a b f es.
    (SintaMaybe f :>> es)
    => S' es f (b -> (a -> b) -> Maybe a -> b)
maybe = unsafeCoerce maybeImpl
    where
        maybeImpl :: S' es f (ANY2 -> (ANY1 -> ANY2) -> Maybe ANY1 -> ANY2)
        maybeImpl = morph3 @"maybe"

runMaybe :: forall f fullEs es n.
    (fullEs ~ SintaMaybe f ++ es)
    => (forall a b. S' fullEs f (b -> (a -> b) -> Maybe a -> Ret b))
    -> Eff fullEs n -> Eff es n
runMaybe handler = runMorph3 @"maybe" handlerImpl
    where
        handlerImpl :: S' fullEs f (ANY2 -> (ANY1 -> ANY2) -> Maybe ANY1 -> ANY2)
        handlerImpl = unsafeCoerce handler

-- *** Ordering

makeMorphs2
    [ "<", "<=", ">", ">=", "compare", "min", "max" ]

infix 4 <, <=, >, >=

type SintaOrd f a =
    SintaEq f a ++
    [ Morph2 "compare" f a a Bool
    , Morph2 "<" f a a Bool
    , Morph2 "<=" f a a Bool
    , Morph2 ">" f a a Bool
    , Morph2 ">=" f a a Bool
    , Morph2 "min" f a a Bool
    , Morph2 "max" f a a Bool ]

-- *** Tuples

makeMorphs
    [ "fst", "snd" ]

type SintaTuple f a b =
    [ Morph "fst" f (a, b) a
    , Morph "snd" f (a, b) b ]

-- *** Enum

makeMorphs
    [ "succ", "pred", "toEnum", "fromEnum", "enumFrom" ]
makeMorphs2
    [ "enumFromThen", "enumFromTo" ]
makeMorphs3
    [ "enumFromThenTo" ]

type SintaEnum f a =
    [ Morph "succ" f a a
    , Morph "pred" f a a
    , Morph "toEnum" f Int a
    , Morph "fromEnum" f a Int
    , Morph "enumFrom" f a [a]
    , Morph2 "enumFromThen" f a a [a]
    , Morph2 "enumFromTo" f a a [a]
    , Morph3 "enumFromThenTo" f a a a [a] ]

-- *** Bound

makeUnbounds
    [ "minBound", "maxBound" ]

type SintaBound f a =
    [ Unbound "minBound" f a
    , Unbound "maxBound" f a ]

--- *** Numeric

makeUnbounds
    [ "pi" ]
makeMorphs
    [ "negate", "abs", "signum", "fromInteger'"
    , "toRational", "toInteger", "recip", "fromRational'"
    , "exp", "log", "sqrt", "sin", "cos", "tan", "asin"
    , "acos", "atan", "sinh", "cosh", "tanh", "asinh"
    , "acosh", "atanh", "properFraction", "truncate"
    , "round", "ceiling", "floor", "floatRadix", "floatDigits"
    , "floatRange", "decodeFloat", "exponent", "significand"
    , "scaleFloat", "isNaN", "isInfinite", "isDenormalized"
    , "isNegativeZero", "isIEEE" ]
makeMorphs2
    [ "+", "-", "*", "/", "**"
    , "div", "mod", "rem", "quot", "quotRem"
    , "divMod", "logBase", "encodeFloat", "atan2" ]

infixl 6 +, -
infixl 7 *, /, `div`, `mod`, `rem`, `quot`
infixr 8 **

type SintaNum f a =
    [ Morph2 "+" f a a a
    , Morph2 "-" f a a a
    , Morph2 "*" f a a a
    , Morph "negate" f a a
    , Morph "abs" f a a
    , Morph "signum" f a a
    , Morph "fromInteger'" f Integer a ]

-- newtype Source t = Source String

-- liftSrc :: (String -> String) -> Source t -> Source u
-- liftSrc f (Source src) = Source (f src)

-- liftSrc2 :: (String -> String -> String) -> Source t -> Source u -> Source v
-- liftSrc2 f (Source aSrc) (Source bSrc) = Source (f aSrc bSrc)

-- runNum :: Eff (SintaNum Source t ++ es) a -> Eff es a
--  =
--     runMorph2 @"+" (liftA2 . liftSrc2 $ \a b -> a ++ "+" ++ b)
--     >>> runMorph2 @"-" (liftA2 . liftSrc2 $ \a b -> a ++ "-" ++ b)
--     >>> runMorph2 @"*" (liftA2 . liftSrc2 $ \a b -> a ++ "*" ++ b)
--     >>> runMorph @"negate" (liftA . liftSrc $ \a -> "negate " ++ a)
--     >>> runMorph @"abs" (liftA . liftSrc $ \a -> "abs " ++ a)
--     >>> runMorph @"signum" (liftA . liftSrc $ \a -> "signum " ++ a)
--     >>> runMorph @"fromInteger'" (liftA . liftSrc $ \a -> "signum " ++ a)

fromInteger ::
    ( Lit f Integer :> es
    , SintaNum f a :>> es )
    => Integer -> Eff' es f a
fromInteger = fromInteger' . lit

type SintaReal f a =
    SintaNum f a ++
    SintaOrd f a ++
    '[ Morph "toRational" f a Rational ]

type SintaIntegral f a =
    SintaReal f a ++
    SintaEnum f a ++
    [ Morph2 "quot" f a a a
    , Morph2 "rem" f a a a
    , Morph2 "div" f a a a
    , Morph2 "mod" f a a a
    , Morph2 "quotRem" f a a (a, a)
    , Morph2 "divMod" f a a (a, a)
    , Morph "toInteger" f a Integer ]

type SintaFractional f a =
    SintaNum f a ++
    [ Morph2 "/" f a a a
    , Morph "recip" f a a
    , Morph "fromRational'" f Rational a ]

fromRational ::
    ( Lit f Rational :> es
    , SintaFractional f a :>> es )
    => Rational -> Eff' es f a
fromRational = fromRational' . lit

type SintaFloating f a =
    SintaFractional f a ++
    [ Unbound "pi" f a
    , Morph "exp" f a a
    , Morph "log" f a a
    , Morph "sqrt" f a a
    , Morph2 "**" f a a a
    , Morph2 "logBase" f a a a
    , Morph "sin" f a a
    , Morph "tan" f a a
    , Morph "asin" f a a
    , Morph "acos" f a a
    , Morph "atan" f a a
    , Morph "sinh" f a a
    , Morph "cosh" f a a
    , Morph "tanh" f a a
    , Morph "asinh" f a a
    , Morph "acosh" f a a
    , Morph "atanh" f a a ]

type SintaRealFrac f i a =
    SintaReal f a ++
    SintaFractional f a ++
    [ Morph "properFraction" f a (i, a)
    , Morph "truncate" f a i
    , Morph "round" f a i
    , Morph "ceiling" f a i
    , Morph "floor" f a i ]

type SintaRealFloat f i a =
    SintaRealFrac f i a ++
    SintaFractional f a ++
    [ Morph "floatRadix" f a Integer
    , Morph "floatDigits" f a Int
    , Morph "floatRange" f a (Int, Int)
    , Morph "decodeFloat" f a (Integer, Int)
    , Morph "exponent" f a Int
    , Morph "significand" f a a
    , Morph2 "scaleFloat" f Int a a
    , Morph "isNaN" f a Bool
    , Morph "isInfinite" f a Bool
    , Morph "isDenormalized" f a Bool
    , Morph "isNegativeZero" f a Bool
    , Morph "isIEEE" f a Bool
    , Morph2 "atan2" f a a a ]

subtract ::
    ( SintaNum f a :>> es )
    => S' es f (a -> a -> Ret a)
subtract = flip (-)

even, odd ::
    ( Lit f Integer :> es
    , SintaIntegral f a :>> es )
    => S' es f (a -> Bool)
even n = n `rem` 2 == 0
odd n = n `rem` 2 /= 0

(^) :: forall a b f es.
    ( Lit f Integer :> es
    , SintaNum f a :>> es
    , SintaIntegral f b :>> es )
    => S' es f (a -> b -> Ret a)
(^) x y =
    if even y
        then (x * x) ^ (y `quot` 2)
        else if y == 1
            then x
            else powAcc (x * x) (y `quot` 2) x
    where
        powAcc :: S' es f (a -> b -> a -> Ret a)
        powAcc x y z =
            if even y
                then powAcc (x * x) (y `quot` 2) z
                else if y == 1
                    then x * z
                    else powAcc (x * x) (y `quot` 2) (x * z)

(^^) ::
    ( Lit f Integer :> es
    , SintaFractional f a :>> es
    , SintaIntegral f b :>> es )
    => S' es f (a -> b -> Ret a)
(^^) x y =
    if y >= 0
        then x ^ y
        else recip (x ^ (negate y))

infixr 8 ^, ^^

-- *** Semigroup and monoids

makeUnbounds
    [ "mempty" ]
makeMorphs
    [ "mconcat" ]
makeMorphs2
    [ "<>" ]

infixr 6 <>

type SintaSemigroup f a =
    [ Morph2 "<>" f a a a
    , Morph "mconcat" f [a] a ]

type SintaMonoid f a =
    SintaSemigroup f a ++
    '[ Unbound "mempty" f a ]

-- *** Monads and functors

data SintaFunctor f g :: Effect where
    Fmap :: m (f (a -> b)) -> m (f (g a)) -> SintaFunctor f g m (f (g b))

makeEffect ''SintaFunctor

data SintaApplicative f g :: Effect where
    Pure :: m (f a) -> SintaApplicative f g m (f (g a))
    Lift :: m (f (a -> b)) -> m (f (g a)) -> SintaApplicative f g m (f (g b))

makeEffect ''SintaApplicative

data SintaMonad f g :: Effect where
    Bind :: m (f (g a)) -> m (f (a -> g b)) -> SintaMonad f g m (f (g b))

makeEffect ''SintaMonad

-- *** List operators

makeMorphs
    [ "fromList'", "toList" ]
makeMorphs2
    [ "!!", "+:", "elem" ]

infixl 9 !!
infixr 5 +:
infix 4 `elem`, `notElem`

type family ListElem l :: Type
type family ListIndex l :: Type

type SintaList f l =
    SintaMonoid f l ++
    [ Morph2 "!!" f l (ListIndex l) (ListElem l)
    , Morph2 "+:" f (ListElem l) l l
    , Morph "fromList'" f [ListElem l] l
    , Morph "toList" f l [ListElem l]
    , Morph2 "elem" f l (ListElem l) Bool ]

(++) ::
    ( SintaList f l :>> es )
    => S' es f (l -> l -> Ret l)
(++) = (<>)

notElem ::
    ( SintaBool f :>> es
    , SintaList f l :>> es )
    => S' es f (l -> ListElem l -> Bool)
notElem l = not . elem l

fromList ::
    ( Lit f [ListElem l] :> es
    , SintaList f l :>> es )
    => [ListElem l] -> Eff' es f l
fromList = fromList' . lit

-- *** String operators

makeMorphs
    [ "fromString'", "lines", "words", "unlines", "unwords" ]

type SintaString f a =
    '[ Morph "fromString'" f String a ]

fromString ::
    ( Lit f String :> es
    , SintaString f a :>> es )
    => String -> Eff' es f a
fromString = fromString' . lit