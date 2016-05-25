-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Data.Zero (
    -- * Semigroups with absorbing element
    Zero(..)
    -- * Num wrappers
  , Product(..)
    -- * Boolean wrappers
  , Any(..)
  , All(..)
    -- * Maybe wrappers
  , Success(..)
  , success
  , failure
  ) where

import Control.Monad.Fix ( MonadFix )
import Data.Monoid ( All(..), Any(..), Product(..) )
import Data.Semigroup ( Semigroup(..) )

-- |'Semigroup' with a 'zero' element. It’s important to understand that the
-- standard 'Semigroup' types – i.e. 'Maybe' and so on – are already biased,
-- because they’re 'Monoid's. That’s why you’ll find a few 'Zero' instances.
--
-- Should satisfies the following laws:
--
-- ==== Annihilation
--
-- @ a '<>' 'zero' = 'zero' '<>' a = 'zero' @
--
-- ==== Associativity
--
-- @ a '<>' b '<>' c = (a '<>' b) '<>' c = a '<>' (b '<>' c) @
class (Semigroup a) => Zero a where
  -- |The zero element.
  zero :: a
  -- |Concat all the elements according to ('<>') and 'zero'.
  zconcat :: [a] -> a

  default zconcat :: [a] -> a
  zconcat [] = zero
  zconcat (x:xs) = foldr (<>) x xs
  {-# MINIMAL zero #-}

instance Zero () where
  zero = ()

instance (Num a) => Zero (Product a) where
  zero = Product 0

instance Zero Any where
  zero = Any True

instance Zero All where
  zero = All False

-- |'Zero' for 'Maybe'.
--
-- Called 'Success' because of the absorbing law:
--
-- @
--   'Success' ('Just' a) '<>' 'Success' 'Nothing' = 'Nothing'
-- @
newtype Success a = Success { getSuccess :: Maybe a }
  deriving (Applicative,Eq,Foldable,Functor,Monad,MonadFix,Ord,Traversable,Read,Show)

instance (Semigroup a) => Semigroup (Success a) where
  Success (Just a) <> Success (Just b) = Success . Just $ a <> b
  _ <> _ = zero

instance (Semigroup a) => Zero (Success a) where
  zero = Success Nothing

-- |A successful value.
success :: a -> Success a
success = Success . Just

-- |A failure.
failure :: Success a
failure = Success Nothing
