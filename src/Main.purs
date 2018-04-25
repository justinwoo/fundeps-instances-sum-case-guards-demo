module Main where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Record as Record
import Type.Equality as Equality
import Type.Prelude (class IsSymbol, class TypeEquals, RLProxy(..), SProxy(..))
import Type.Row (kind RowList)
import Type.Row as Row

-- values of a homogeneous record
class Values (xs :: RowList) (row :: # Type) a
  | xs -> row a
  -- used for matching -> "unrestricted" bindings that we use
  where
    values :: RLProxy xs -> { | row } -> List a

instance nilValues ::

  -- we can explicitly use constraints here
  -- ( TypeEquals { | row } {}
  -- ) => Values Row.Nil row a where

  -- ( RowEquals row ()
  -- ) => Values Row.Nil row a where

  -- or just match them in the instance head directly
  Values Row.Nil () a where
  values _ _ = List.Nil

-- silly row equals type class we can define for convenience above
-- class TypeEquals {|r1} {|r2}
--    <= RowEquals (r1 :: # Type) (r2 :: # Type)

instance consValues ::
  ( IsSymbol name
  , Values tail row a

  -- like guards, we can add the constraints here instead of matching
  -- on the instance head, but it's more work overall
  -- , TypeEquals a ty
  -- , RowCons name ty row' row
  -- ) => Values (Row.Cons name ty tail) row a where

  -- or just match in the instance head and directly use everything
  , RowCons name a row' row
  ) => Values (Row.Cons name a tail) row a where
  values _ r = List.Cons first rest
    where
      -- needed if we don't match the type in the instance head
      -- because then we need to apply the type equality
      -- first = Equality.from $ Record.get (SProxy :: SProxy name) r

      first = Record.get (SProxy :: SProxy name) r
      rest = values (RLProxy :: RLProxy tail) r

-- type class instances can be "partial" and this is used to make sure
-- not every type has an instance of your class (which is what you want)
addOnesPartial :: Partial => Int -> List Int -> Int
addOnesPartial acc xs = case xs of

  List.Cons x tail
    | x == 1
    -> addOnes (x + acc) tail

  -- or match with the literal value like in instance heads
  -- List.Cons 1 tail
  --   -> addOnes (1 + acc) tail

  List.Nil -> acc

-- but term level partiality is annoying, so we often don't do that here
addOnes :: Int -> List Int -> Int
addOnes acc xs = case xs of
  List.Cons x tail
    | x == 1
    -> addOnes (x + acc) tail
  List.Cons x tail
    -> addOnes acc tail
  List.Nil -> acc
