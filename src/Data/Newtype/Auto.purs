module Data.Newtype.Auto where

import Prelude (identity, (<<<))

import Data.Newtype (class Newtype, unwrap, wrap)

-- Only using a to allow the superclass constraint
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

modify :: forall t o a. AutoNewtype t o a => (Auto t -> o) -> t -> t
modify f = autoWrap <<< pretend f

modify' :: forall t o o' a. AutoNewtype t o a => AutoNewtype t o' a => (o -> o') -> t -> t
modify' f = autoWrap <<< f <<< autoUnwrap
