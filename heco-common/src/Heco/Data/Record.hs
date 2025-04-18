module Heco.Data.Record where

import GHC.Generics ((:*:), M1, K1, D, C, S, R, selName, Selector, Rep)
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable, Proxy(..), TypeRep, typeRep)

data FieldInfo = FieldInfo
    { name :: String
    , typeRep :: TypeRep }

class RecordFields rep where
    recordFieldsImpl :: [FieldInfo]

instance RecordFields f => RecordFields (M1 D x f) where
    recordFieldsImpl = recordFieldsImpl @f

instance RecordFields f => RecordFields (M1 C x f) where
    recordFieldsImpl = recordFieldsImpl @f

instance (Selector s, Typeable t) => RecordFields (M1 S s (K1 R t)) where
    recordFieldsImpl =
        [FieldInfo
            { name = selName (undefined :: M1 S s (K1 R t) ())
            , typeRep = typeRep (Proxy :: Proxy t) }]

instance (RecordFields a, RecordFields b) => RecordFields (a :*: b) where
    recordFieldsImpl = recordFieldsImpl @a ++ recordFieldsImpl @b

instance RecordFields M1 where
    recordFieldsImpl = []

recordFields :: forall t. RecordFields (Rep t) => [FieldInfo]
recordFields = recordFieldsImpl @(Rep t)

data FieldInfoEx ex = FieldInfoEx
    { name :: String
    , typeRep :: TypeRep
    , extra :: ex }

class RecordFieldsEx (c :: Type -> Constraint) ex rep where
    recordFieldsExImpl :: (forall f. c f => Proxy f -> ex) -> [FieldInfoEx ex]

instance RecordFieldsEx c ex f => RecordFieldsEx c ex (M1 D x f) where
    recordFieldsExImpl = recordFieldsExImpl @c @ex @f

instance RecordFieldsEx c ex f => RecordFieldsEx c ex (M1 C x f) where
    recordFieldsExImpl = recordFieldsExImpl @c @ex @f

instance (Selector s, Typeable t, c t) => RecordFieldsEx c ex (M1 S s (K1 R t)) where
    recordFieldsExImpl f =
        [ FieldInfoEx
            { name = selName (undefined :: M1 S s (K1 R t) ())
            , typeRep = typeRep (Proxy :: Proxy t)
            , extra = f (Proxy :: Proxy t) } ]

instance (RecordFieldsEx c ex a, RecordFieldsEx c ex b) => RecordFieldsEx c ex (a :*: b) where
    recordFieldsExImpl f = recordFieldsExImpl @c @ex @a f ++ recordFieldsExImpl @c @ex @b f

instance RecordFieldsEx c ex M1 where
    recordFieldsExImpl _ = []

recordFieldsEx :: forall t c ex. RecordFieldsEx c ex (Rep t)
    => (forall f. c f => Proxy f -> ex)
    -> [FieldInfoEx ex]
recordFieldsEx f = recordFieldsExImpl @c @ex @(Rep t) f