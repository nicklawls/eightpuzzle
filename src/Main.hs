module Main where

import System.Environment
import Data.Dequeue
import Control.Monad

{- We'll start by defining a few abstract building blocks for pieces of the general
   search algorithm. Allowing us to generalize to any NPuzzle, or any other
   search problem, as long as we can implement this interface

   We'll then define concrete datatypes for the eight puzzle problem
   and connect them to the abstract interface.
-}

{- Since the state of the problem can in principle be any datatype, we'll
   represent it with a a type variable ;a'. The keyword 'type' denotes a type
   synonym, letting us use 'State a' in place of a plain 'a', making our code
   follow the languge of the problem a bit more closely
-}

type State a = a

{- an Operator for type 'a' is simply a function from 'State a' to a list of 'State a's
   i.e. from a current state to a set of new states
-}

type Operator a =  State a -> [State a]

{- this is a typeclass declaration, basically saying that a type 'a' represets
   a Problem if the two functions 'expand' and 'isGoal' are defined.
   We will define these functions for EightPuzzle in an "instance" declaration
   in a few lines

   Note that even with this highly general definition, we can already implement
   expand for any possible 'a'! Simply apply all the operators to the state,
   and stitch together the resultant lists of states. Haskell's typeclass
   syntax allows us to a provide a default implementation in the declaration.
-}

class Problem a where
    expand :: State a -> [Operator a] -> [State a]
    isGoal :: State a -> Bool
    expand state operators =
        concat (applyAll operators state)
        -- reads: "concatinate the results of applying all operators to the state "

-- TODO implement this with a fold
applyAll :: [a -> b] -> a -> [b]
applyAll fns state =
    case fns of
        [] -> []
        (f:fs) -> f state : applyAll fs state

-- State is the type representing the problem state
-- Queue State is , well, a queue of states
-- generalSearch :: Problem a =>  -> (Queue State -> State -> Queue State ) -> Either String [a]
-- isGoal :: State -> Bool
-- expand :: State -> [State -> State] -> [State]

main :: IO ()
main = do
  args <- getEnv "foo"
  putStrLn "hello world"
