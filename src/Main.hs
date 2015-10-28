module Main where

import System.Environment
import Control.Monad
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.PQueue.Min

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
   a Problem if it is an instance of the 'Ord' class, and if the two functions
   'expand' and 'isGoal' and the value 'initialState' are defined. We will
   define these functions for EightPuzzle in an 'instance' declaration later on.

   Note that even with this highly general definition, we can already implement
   expand for any possible 'a'! Simply apply all the operators to the state,
   and stitch together the resultant lists of states. Haskell's typeclass
   syntax allows us to a provide a default implementation in the declaration.
-}

class Ord a => Problem a where
    expand :: State a -> [State a]
    isGoal :: State a -> Bool
    operators :: [Operator a]
    expand state =
        concat (applyAll operators state)
        -- reads: "concatinate the results of applying all operators to the state "

-- apply each function in a list of functions to some value, returning a list of results
-- TODO implement this with a fold
applyAll :: [a -> b] -> a -> [b]
applyAll functions state =
    case functions of
        [] -> []
        (firstFunction:rest) -> firstFunction state : applyAll rest state



{-  The queueing function takes the current queue and the expanded states, and
    inserts them into the queue in some order. The order of insertion determines
    the nature of the search algorithm. For our class of problems, we'll use a
    min priority queue.
-}

type Queue a = MinQueue a
type QueueingFunction a = Queue (State a) -> [State a] -> Queue (State a)

{- We can finally write the general search algorithm, which takes any 'State a'
   implementing the 'Problem' interface, plus a queueing function with which to
   order the insertion into the search queue.

   As in the psuedocode, we pass in an initial problem state and a queueing
   function to order node insertion
   We return an 'Either' a 'String' to denote that the operation might fail,
   producing an error 'String', or the correct solution state
-}

generalSearch :: Problem a => State a -> QueueingFunction a -> Either String (State a)
generalSearch state = go (singleton state)
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


-- the second argument is the complete function h(x)
-- we return a complete queueing function cooresponding to the a* algorithm
astar :: Problem a => (State a -> Int) -> Queue (State a) -> [State a] -> Queue (State a)
astar h queue nodes = undefined




uniformCost :: Problem a => QueueingFunction a
uniformCost = astar  (const 0) -- 'const 0' is a function that always returns 0

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

data Cell
    = Blank
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight

{- The puzzle itself is simply a mapping
-}

-- TODO EightPuzzle

{-
-}


-- astarMisplacedTile :: QueueingFunction EightPuzzle
-- astarMisplacedTile = undefined
--
-- astarManhattan :: Problem a => QueueingFunction EightPuzzle
-- astarManhattan = undefined






main :: IO ()
main = do
  args <- getEnv "foo"
  putStrLn "hello world"
