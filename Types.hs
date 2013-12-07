module Types(
    Car(..),
    Shape,
) where

import qualified Data.Foldable as F
import Data.Monoid

data Car = Car {
    make :: Maybe String,
    model :: String,
    year :: Maybe Int,
    color :: String
} deriving (Show)

data Point a b = Point a b deriving (Show)
data Shape a b c = Circle (Point a b) c | Rectangle (Point a b) (Point a b) deriving (Show)

data Shape' a  b c = Circle' {center :: Point a b, r :: c} | Rectangle' {fst :: Point a b, snd :: Point a b}

tellShape :: Shape Float Float Int -> String
tellShape (Circle (Point a b) c) = "Origin: " ++ (show a) ++ ", " ++ (show b) ++ " r: " ++ (show c)

{-data List a = Empty | Cons a (List a) deriving (Show, Eq, Read, Ord)-}

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Eq, Read, Ord, Show)

{-infixr 5 +.+-}
{-(+.+) :: [a] -> [a] -> [a]-}
{-[] +.+ ys = ys-}
{-(x:xs) +.+ ys = x:(xs +.+ ys)-}

infixr 5 +.+
(+.+) :: List a -> List a -> List a
Empty +.+ ys = ys
(x :-: xs) +.+ ys = x :-: (xs +.+ ys)

foo :: String -> String
foo "a" = "DUDE!"
foo xs = "Whatever"

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree EmptyTree x = Node x EmptyTree EmptyTree
insertTree (Node t left right) x
    | x == t = Node t left right
    | x < t =  Node t (insertTree left x) right
    | otherwise = Node t left (insertTree right x)

treeElem :: (Ord a) => a -> Tree a -> Bool
a `treeElem` EmptyTree = False
a `treeElem` (Node x left right)
    | a == x = True
    | a < x = a `treeElem` left
    | otherwise = a `treeElem` right

instance Functor Tree where
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
    fmap f EmptyTree = EmptyTree

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x left right) = f x `mappend`
        F.foldMap f left `mappend`
        F.foldMap f right

{-let testTree = Node 5 (Node 3  (Node 1 EmptyTree EmptyTree)  (Node 6 EmptyTree EmptyTree)  )  (Node 9  (Node 8 EmptyTree EmptyTree)  (Node 10 EmptyTree EmptyTree)  )-}
