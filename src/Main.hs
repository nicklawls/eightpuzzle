module Main where

import System.Environment
import Control.Monad
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.PQueue.Prio.Min (MinPQueue, singleton, minView, insert)

{-  We'll start by defining a few abstract building blocks for pieces of the general
    search algorithm. Allowing us to generalize to any NPuzzle, or any other
    search problem, as long as we can implement this interface

    We'll then define concrete datatypes for the eight puzzle problem
    and connect them to the abstract interface.
-}

{-  In order to provide a generic interface for other search problem, we'll be
    making use of Haskell's hefty type system. See the follwoing link if you get
    confused: http://joelburget.com/data-newtype-instance-class/
-}


{-  An Operator for type 'state' is simply a function from a 'state' to a list of 'states's
    i.e. from a current state to a set of child states. Each operator also has
    an associated cost which we can represent as simply an integer
-}


type Cost = Int -- 'Cost' is now an alias for the primitive type 'Int'

type Operator state = ( Cost, state -> [state] )
-- 'state' is a type variable that any type can inhabit
-- 'a -> b' defines a function from a to b
-- (a,b) is a tuple with inhabitants of possibly different type

{- Below is a typeclass declaration, basically saying that a type 'a' represets
   a 'Problem' if it has a set of 'Operators', and if the two functions
   'expand' and 'isGoal' are defined. We will define these functions for
   'EightPuzzle' in an 'instance' declaration later on.

   Note that even with this highly general definition, we can already implement
   expand for any possible 'state'! Simply apply all the operators to the state,
   and stitch together the resultant lists of states. Haskell's typeclass
   syntax allows us to a provide a default implementation in the declaration.
-}

class Problem state where
    isGoal :: state -> Bool
    operators :: [Operator state]
    expand :: state -> [state]
    expand state =
        concat (applyAll (map snd operators) state)
        -- reads: "concatinate the results of applying all operators to the state "

-- apply each function in a list of functions to some value, returning a list of results
applyAll :: [a -> b] -> a -> [b]
applyAll functions state =
    case functions of
        [] -> []
        (firstFunction:rest) -> firstFunction state : applyAll rest state
-- 'state' here is a value-level function argument, not a type variable


{-  The queueing function takes the current queue and the expanded states, and
    inserts them into the queue in some order. The order of insertion determines
    the nature of the search algorithm. For our class of problems, we'll use a
    min priority queue that will dequeue its entries in order of increasing 'Cost'.

-}

type Queue state = MinPQueue Cost state
-- forces us to use 'Int' valued Cost for all problems

type QueueingFunction state = Queue state -> [state] -> Queue state

{- We can finally write the general search algorithm, which takes anything of type 'state'
   implementing the 'Problem' interface, plus a queueing function with which to
   order the insertion into the search queue.

   As in the psuedocode, we pass in an initial problem state and a queueing
   function to order node insertion
   We return an 'Either' a 'String' to denote that the operation might fail,
   producing an error 'String', or the correct solution state

   Haskell code doesn't have an immediately obvious way to express while loops,
   so we use the recursive 'go' function to implement iteration. The 'go'
   function also uses the 'do' syntax sugar to look more imperative
-}

generalSearch :: Problem state => state -> QueueingFunction state -> Either String state
generalSearch state =
    go (singleton 0 state) -- start with initial cost 0
        where go nodes qf = do
                (node, queue) <- case minView nodes of
                                    Nothing -> Left "No Solution"
                                    Just (node,queue) -> Right (node,queue)
                if isGoal node
                  then return node
                  else go (qf queue (expand node) ) qf




{- Since every one of our search procedures is a derivative of A*, we can define a
   general version first, then pass the heuristic function as an argument.

   We can also implement Uniform Cost search here, since it uses h(n) = 0 no
   matter what the problem is.
-}

-- TODO fix when cost is tupled
astar :: Problem state => (state -> Cost) -- the function g(x)
                       -> (state -> Cost) -- the function h(x)
                       -> Queue state     -- initial queue
                       -> [state]         -- expanded nodes
                       -> Queue state     -- new queue
astar g h queue nodes =
    let f x = g x + h x
    in  insertAll queue (map (\node -> (f node, node)) nodes)


insertAll :: Problem state => Queue state -> [(Cost,state)] -> Queue state
insertAll = foldl' (flip (uncurry insert))
-- this magical incantation inserts the list of (score, node) pairs into the queue


-- TODO Fix this when cost is tupled
uniformCost :: Problem state => QueueingFunction state
uniformCost = astar (const 1) (const 0) -- 'const 0' is a function that always returns 0

{- Now that we have a general framework set up, lets define the Eight Puzzle
   problem by defining its state and its operators.

   We saw in class that a minimal set of operators will reduce the branching
   factor, thereby making the problem easier to solve. In the case of the eight
   puzzle, or any n-puzzle, the minimal set of operators are the 4 operators
   that move the blank around. Therefore, we need a data structure that allows
   these operators to do their work efficiently
-}

{- Each Cell of the puzzle can be in one of nine possible states, represented
   by the following Haskell algebraic datatype.
-}

-- data Cell
--     = Blank
--     | One
--     | Two
--     | Three
--     | Four
--     | Five
--     | Six
--     | Seven
--     | Eight

{-  Data structure: start with the operators to design it

-}

-- TODO EightPuzzle

{-
-}


-- astarMisplacedTile :: QueueingFunction EightPuzzle
-- astarMisplacedTile = undefined
--
-- astarManhattan :: QueueingFunction EightPuzzle
-- astarManhattan = undefined






main :: IO ()
main = do
  args <- getEnv "foo"
  putStrLn "hello world"
