module Heco.Data.Typelits where

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(Proxy))

class KnownSymbols ss where
    symbolValues :: [String]

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolValues = symbolVal (Proxy :: Proxy s) : symbolValues @ss

instance KnownSymbols '[] where
    symbolValues = []