{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Histo where


import Prelude hiding (lookup)
import Control.Arrow ( (>>>) )
import Data.List (partition)


newtype Fix f = Fix { unFix :: f (Fix f) }


data Attr f a = Attr
              { attribute :: a
              , hole      :: f (Attr f a)
              }


type CVAlgebra f a = f (Attr f a) -> a


histo :: Functor f => CVAlgebra f a -> Fix f -> a
histo h = unFix >>> fmap worker >>> h where
  worker t = Attr (histo h t) (fmap worker (unFix t))


type Cent = Int

coins :: [Cent]
coins = [50, 25, 10, 5, 1]


data NatF a
    = Zero
    | Succ a
    deriving Functor


type Nat = Fix NatF


one, two, three :: Nat
one   = Fix (Succ (Fix Zero))
two   = Fix (Succ one)
three = Fix (Succ two)


-- Puts all the Fixes in
expand :: Int -> Nat
expand 0 = Fix Zero
expand n = Fix (Succ (expand (n - 1)))


compress :: NatF (Attr NatF a) -> Int
compress Zero              = 0
compress (Succ (Attr _ x)) = 1 + compress x


change :: Cent -> Int
change amt = histo go (expand amt) where
  go :: NatF (Attr NatF Int) -> Int
  go Zero = 1
  go curr@(Succ attr) = let
    given               = compress curr
    validCoins          = filter (<= given) coins
    remaining           = (given -) <$> validCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum $ lookup attr <$> toProcess
    in length zeroes + results


lookup :: Attr NatF a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where (Succ inner) = hole cache