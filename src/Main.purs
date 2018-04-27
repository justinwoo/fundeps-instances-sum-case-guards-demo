module Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.List (List)
import Data.List as List
import Data.Record as Record
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Partial.Unsafe (unsafePartial)
import Type.Equality as Equality
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, class TypeEquals, RLProxy(..), SProxy(..))
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
class TypeEquals {|r1} {|r2}
   <= RowEquals (r1 :: # Type) (r2 :: # Type)

instance consValues ::
  ( IsSymbol name
  , Values tail row' a

  -- like guards, we can add the constraints here instead of matching
  -- on the instance head, but it's more work overall
  -- , TypeEquals a ty
  -- , RowCons name ty row' row
  -- , RowLacks name row'
  -- ) => Values (Row.Cons name ty tail) row a where

  -- or just match in the instance head and directly use everything
  , RowCons name a row' row
  , RowLacks name row'
  ) => Values (Row.Cons name a tail) row a where
  values _ r = List.Cons first rest
    where
      nameP = SProxy :: SProxy name

      -- needed if we don't match the type in the instance head
      -- because then we need to apply the type equality
      -- first = Equality.from $ Record.get nameP r

      first = Record.get nameP r

      r' = Record.delete nameP r :: { | row' }
      rest = values (RLProxy :: RLProxy tail) r'

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

matchEmpty :: Partial => List Int -> StrMap Int -> Boolean
matchEmpty xs r = case xs, r of
  List.Nil, r' | r' == empty' ->
    false
  where
    empty' = StrMap.empty

values'
  :: forall xs row a
   . RowToList row xs
  => Values xs row a
  => { | row }
  -> List a
values' = values (RLProxy :: RLProxy xs)

main = unsafePartial do
  -- (1 : 2 : Nil)
  log $ show $ values' {x: 1, y: 2}

  -- compile error: can't match Int with String when solving
  -- log $ show $ values' {x: "A", y: 2}

  -- 1
  log $ show $ addOnesPartial 0 (List.Cons 1 List.Nil)

  -- crashes with partiality
  -- log $ show $ addOnesPartial 0 (List.Cons 2 List.Nil)

  -- returns false
  log $ show $ matchEmpty List.Nil StrMap.empty

  -- this crashes with partiality
  -- log $ show $ matchEmpty (List.Cons 1 List.Nil) StrMap.empty
