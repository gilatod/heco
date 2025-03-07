module Heco.Data.Record (recordFields, RecordFields) where

import GHC.Generics ((:*:), M1, K1, D, C, S, R, selName, Selector, Rep)

class RecordFields rep where
    recordFieldsImpl :: [String]

instance RecordFields f => RecordFields (M1 D x f) where
    recordFieldsImpl = recordFieldsImpl @f

instance RecordFields f => RecordFields (M1 C x f) where
    recordFieldsImpl = recordFieldsImpl @f

instance (Selector s) => RecordFields (M1 S s (K1 R t)) where
    recordFieldsImpl =
        [selName (undefined :: M1 S s (K1 R t) ())]

instance (RecordFields a, RecordFields b) => RecordFields (a :*: b) where
    recordFieldsImpl = recordFieldsImpl @a ++ recordFieldsImpl @b

instance RecordFields M1 where
    recordFieldsImpl = []

recordFields :: forall t. RecordFields (Rep t) => [String]
recordFields = recordFieldsImpl @(Rep t)