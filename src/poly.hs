module Main where
import Prelude (String, Int, Num(..), Float, Bool(..))
import System.IO

-- {BEGIN: maybe}

--------------------------------------------------------------------------------
-- | The 'Maybe' polymorphic sum type.
data Maybe a = Nothing | Just a

--------------------------------------------------------------------------------
-- | Optionally returning a value with 'Maybe'.
shoeSize :: String -> Maybe Float
shoeSize "Peter" = Just 8.0
shoeSize _       = Nothing

--------------------------------------------------------------------------------
-- | Pattern matching with @case@.
hasShoeSize :: String -> Bool
hasShoeSize name =
  case shoeSize name of
    Nothing -> False
    Just _  -> True

-- {END}
-- {BEGIN: either}

--------------------------------------------------------------------------------
-- | The 'Either' type.
data Either a b = Left a | Right b

-- {END}
-- {BEGIN: tree}

--------------------------------------------------------------------------------
-- | Simple binary tree containing leafs and nodes.
data Tree a = Leaf | Node a (Tree a) (Tree a)

--------------------------------------------------------------------------------
-- | Tree:
--                  (1)
--                 /  \
--               (2)  (L)
--              /  \
--            (L)  (3)
--                /  \
--              (L)  (L)
--
tree :: Tree Int
tree = Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) Leaf

--------------------------------------------------------------------------------
-- | Sum a tree of numbers.
sum :: (Num a) => Tree a -> a
sum Leaf                = 0
sum (Node x left right) = x + sum left + sum right

-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = print (sum tree)
