-- Type classes for Boolean algebra and Boolean values (bits), see #86.

module Foundation.Boolean
    ( Bool (..)
    , Boolean (..)
    , Bit (..)
    ) where

import Data.List
import Data.Bool (Bool)
import qualified Data.Bool
import qualified Prelude

class Bit a where
    bool :: b -> b -> a -> b

instance Bit Bool where
    bool = Data.Bool.bool

class Boolean a where
    {-# MINIMAL (true | false), not, ((&&) | (||)) #-}

    true  :: a
    false :: a
    not   :: a -> a
    (&&)  :: a -> a -> a
    (||)  :: a -> a -> a

    true   = not false
    false  = not true
    x && y = not (not x || not y)
    x || y = not (not x && not y)

instance Boolean Bool where
    true  = Data.Bool.True
    false = Data.Bool.False
    not   = Data.Bool.not
    (&&)  = (Data.Bool.&&)
    (||)  = (Data.Bool.||)

instance Boolean a => Boolean [a] where
    true  = repeat true
    false = repeat false
    not   = map not
    (&&)  = zipWith (&&)
    (||)  = zipWith (||)

instance (Boolean a, Boolean b) => Boolean (a, b) where
    true               = (true   , true   )
    false              = (false  , false  )
    not (a, b)         = (not a  , not b  )
    (a, b) && (a', b') = (a && a', b && b')
    (a, b) || (a', b') = (a || a', b || b')

instance Boolean a => Boolean (b -> a) where
    true   = Prelude.const true
    false  = Prelude.const false
    not f  = \x -> not (f x)
    f && g = \x -> f x && g x
    f || g = \x -> f x || g x
