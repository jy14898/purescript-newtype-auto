module Data.Newtype.Auto where

import Prelude (identity, (<<<))

import Data.Newtype (class Newtype, unwrap, wrap)

-- Only using a to allow the superclass constraint
-- Is it worth it?
-- Also should I flip the order of o and a

-- | A type class similar to `Data.Newtype.Newtype`, however, unwrapping may
-- | return the wrapped type if the use of the result expects it.
-- |
-- | Note that any instance will overlap with the two instances defined here,
-- | so instances of this class should not be defined in libraries.
class (Newtype t a) <= AutoNewtype t o a
    | t -> a
    , o -> a where
    autoUnwrap :: t -> o
    autoWrap   :: o -> t

instance autoNewtypeIdentity :: 
    ( Newtype t a
    ) => AutoNewtype t t a where
    autoUnwrap = identity
    autoWrap   = identity

else instance autoNewtypeNewtype :: 
    ( Newtype t a
    ) => AutoNewtype t a a where
    autoUnwrap = unwrap
    autoWrap   = wrap

type Auto t = forall o a. AutoNewtype t o a => o

pretend :: forall t r. (Auto t -> r) -> t -> r
pretend f v = f (autoUnwrap v)

-- problem with this is that if you pass in identity
-- o ~ Auto t
-- and then we're looking up AutoNewtype t (Auto t) a
-- which I have no idea if it would work when the apartness bug is fixed
modify :: forall t o a. AutoNewtype t o a => (Auto t -> o) -> t -> t
modify f = autoWrap <<< pretend f

modify' :: forall t o o' a. AutoNewtype t o a => AutoNewtype t o' a => (o -> o') -> t -> t
modify' f = autoWrap <<< f <<< autoUnwrap
