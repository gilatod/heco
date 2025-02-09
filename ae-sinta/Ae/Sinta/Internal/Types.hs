module Ae.Sinta.Internal.Types where

import Effectful ( (:>), Eff )
import Data.Kind ( Constraint, Type )

-- * Auxiliary types

type Eff' es f t = Eff es (f t)

data Ret t

type family S' es f (t :: Type) :: Type where
    S' es f (a -> b) = Eff es (f a) -> S' es f b
    S' es f (Ret r) = Eff es (f r)
    S' es f r = Eff es (f r)

type family xs :>> es :: Constraint where
    '[] :>> es = ()
    '[e] :>> es = (e :> es)
    [e1, e2] :>> es = (e1 :> es, e2 :> es)
    [e1, e2, e3] :>> es = (e1 :> es, e2 :> es, e3 :> es)
    [e1, e2, e3, e4] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es)
    [e1, e2, e3, e4, e5] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es)
    [e1, e2, e3, e4, e5, e6] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es)
    [e1, e2, e3, e4, e5, e6, e7] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es, e13 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es, e13 :> es, e14 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es, e13 :> es, e14 :> es, e15 :> es)
    [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16] :>> es = (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es, e13 :> es, e14 :> es, e15 :> es, e16 :> es)
    (e1 : e2 : e3 : e4 : e5 : e6 : e7 : e8 : e9 : e10 : e11 : e12 : e13 : e14 : e15 : e16 : rs) :>> es = 
        (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e8 :> es, e9 :> es, e10 :> es, e11 :> es, e12 :> es, e13 :> es, e14 :> es, e15 :> es, e16 :> es, rs :>> es)