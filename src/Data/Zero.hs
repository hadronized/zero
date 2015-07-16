-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Data.Zero where

import Control.Monad.Fix ( MonadFix )
import Data.Semigroup ( Semigroup(..) )
import GHC.Generics ( Generic )

-- |'Semigroup' with a 'zero' element. It’s important to understand that the
-- standard 'Semigroup' types – i.e. 'Maybe' and so on – are already biased,
-- because they’re 'Monoid's. That’s why you’ll find a few 'Zero' instances.
--
-- Should satisfies the following laws:
--
-- ==== Annhilation
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

  default zconcat :: (Semigroup a) => [a] -> a
  zconcat [] = zero
  zconcat (x:xs) = foldr (<>) x xs
  {-# MINIMAL zero #-}

instance Zero () where
  zero = ()

-- |'Zero' under multiplication.
newtype Product a = Product { getProduct :: a }
  deriving (Bounded,Eq,Generic,Num,Ord,Read,Show)

instance (Num a) => Semigroup (Product a) where
  Product a <> Product b = Product $ a * b

instance (Num a) => Zero (Product a) where
  zero = Product 0

-- |'Zero' under boolean logical and.
newtype All = All { getAll :: Bool} deriving (Bounded,Eq,Generic,Ord,Read,Show)

instance Semigroup All where
  All a <> All b = All $ a && b

instance Zero All where
  zero = All False

-- |'Zero' for 'Maybe'.
newtype Success a = Success { getSuccess :: Maybe a }
  deriving (Applicative,Eq,Foldable,Functor,Monad,MonadFix,Ord,Traversable,Read,Show)

instance (Semigroup a) => Semigroup (Success a) where
  Success (Just a) <> Success (Just b) = Success . Just $ a <> b
  _ <> _ = zero

instance (Semigroup a) => Zero (Success a) where
  zero = Success Nothing
