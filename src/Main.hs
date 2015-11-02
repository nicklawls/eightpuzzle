module Main where

import System.Environment
import Data.Maybe (catMaybes)
import Control.Monad
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.PQueue.Prio.Min (MinPQueue, singleton, minView, insert)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V (elemIndex, findIndex, toList, indexed, fromList, length, zipWith, filter)
import qualified Data.Matrix as M (fromList)

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
    i.e. from a current state to a child state. For simplicity, we trust that
    operators will apply their costs to the 'state'
-}


type Cost = Int -- 'Cost' is now an alias for the primitive type 'Int'

type Operator state = state -> Maybe state
-- 'state' is a type variable that any type can inhabit
-- 'a -> b' defines a function from a to b


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
        catMaybes (applyAll operators state)
        -- reads apply all operators to the state, leaving out
        -- the operators that didn't apply


applyAll :: [a -> b] -> a -> [b]
applyAll functions state =
    case functions of
        [] -> []
        (firstFunction:rest) -> firstFunction state : applyAll rest state
-- apply each function in a list of functions to some value, returning a list of results
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
    go (singleton 0 state) -- start with initial cost 0, singleton initializes queue
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
astar :: Problem state => (state -> Cost) -- the function h(x)
                       -> (state -> Cost) -- the function g(x)
                       -> Queue state     -- initial queue
                       -> [state]         -- expanded nodes
                       -> Queue state     -- new queue
astar h g queue nodes =
    let f x = g x + h x
    in  insertAll queue (map (\node -> (f node, node)) nodes)


insertAll :: Problem state => Queue state -> [(Cost,state)] -> Queue state
insertAll = foldl' (flip (uncurry insert))
-- this magical incantation inserts the list of (score, node) pairs into the queue


-- TODO Fix this when cost is tupled
uniformCost :: Problem state => (state -> Cost) -> QueueingFunction state
uniformCost = astar (const 0) -- 'const 0' is a function that always returns 0

{- Now that we have a general framework set up, lets define the Eight Puzzle
   problem by defining its state and its operators.

   We saw in class that a minimal set of operators will reduce the branching
   factor, thereby making the problem easier to solve. In the case of the eight
   puzzle, or any n-puzzle, the minimal set of operators are the 4 operators
   that move the blank around. Therefore, we need a data structure that allows
   these operators to do their work efficiently
-}


{-  To represent the eight puzzle, we use a record that contains both the depth
    of the solution and vector where v(i) current position of tile i. If make it
    a convention to make v(0) the location of the blank, we can quickly (O(1))
    locate the position of the blank tile at any time, an operation we have to
    perfrom on every iteration. We must then pay O(n) time, where n is the size
    of the puzzle, to search for the blank's neighbor and perform the swap, but
    this operation is not needed if the operator doesn't apply to the tile.
-}

type Position = Int


data EightPuzzle = EightPuzzle
    { depth :: Cost -- gives you 'depth :: EightPuzzle -> Cost' for free, will become g(x)
    , board :: Vector Position
    } deriving (Eq)


-- allows us to keep the inverted representation while
-- pretty printing the actual board structure
instance Show EightPuzzle where
    show puzzle =
        ( show
        . M.fromList 3 3
        . map fst
        . sortBy (comparing snd)
        . V.toList
        . V.indexed
        . board
        $ puzzle
        ) ++ "Depth: " ++ show (depth puzzle)


{-  To implement the Operators for Eightpuzzle, we can begin by defining a
    general function that will move the blank in any direction, with the direction
    specified by a function that finds the new position of the blank and a function
    that checks that the proposed move can occur
-}


moveBlank :: (Position -> Position) -- function to find the neightbor
          -> (Position -> Bool) -- predicate for moveability in direction x
          -> EightPuzzle
          -> Maybe EightPuzzle
moveBlank calcNeighbor boundaryTest (EightPuzzle depth board) =
    let blank = board ! 0
        Just neighbor = V.elemIndex (calcNeighbor blank) board
    in
        if boundaryTest blank
          then Just
                EightPuzzle
                    { depth = depth + 1
                    , board = swap board 0 neighbor
                    }
          else Nothing
-- (!) is unsafe vector indexing
-- the (Just neighbor) pattern match will throw an exception if elemIndex returns
-- 'Nothing'


-- swap the element at index1 with the element at index2
swap :: Vector a -> Int -> Int -> Vector a
swap vec index1 index2 =
    vec //
        [ (index1, vec ! index2)
        , (index2, vec ! index1)
        ]



{-  With the general framework in place, we can specialize it for each direction
-}

moveBlankLeft :: Operator EightPuzzle
moveBlankLeft = moveBlank (\x -> x - 1) $ (> 0) . (`mod` 3)


moveBlankRight :: Operator EightPuzzle
moveBlankRight = moveBlank (+ 1) $ (< 2) . (`mod` 3)


moveBlankUp :: Operator EightPuzzle
moveBlankUp = moveBlank (\x -> x - 3) (> 2)


moveBlankDown :: Operator EightPuzzle
moveBlankDown = moveBlank (+ 3) (< 6)


instance Problem EightPuzzle where
    isGoal    = (== goalBoard ) . board
    operators =
        [ moveBlankUp
        , moveBlankDown
        , moveBlankLeft
        , moveBlankRight
        ]


goalBoard :: Vector Position
goalBoard = V.fromList (8:[0..7])


-- function to get a list of ints into the puzzle representation

makePuzzle :: [Int] -> EightPuzzle
makePuzzle tiles =
    EightPuzzle 0 $
        V.fromList $ map snd $ sortBy (comparing fst) $ zip tiles [0..8]

-- sample puzzles for testing
puzzle :: EightPuzzle
puzzle = makePuzzle [8,3,1,2,0,7,4,5,6]

puzzle2 :: EightPuzzle
puzzle2 = makePuzzle [8,3,1,2,6,7,4,5,0]

{- Finally, we can specialize the search algorithm that we defined earlier by
   Plugging in the appropriate heuristics and depth functions
-}




misplacedTile :: EightPuzzle -> Cost
misplacedTile puzzle =
    let countMisplaced = V.length . V.filter (== False) . V.zipWith (==) goalBoard . board
    in
        if board puzzle ! 0 == 8
            then countMisplaced puzzle -- when the blank is in place, count up the misplaced tiles
            else countMisplaced puzzle - 1 -- when it isnt, count up and subtract the non-existant tile


-- min 8 to discount cases

manhattan :: EightPuzzle -> Cost
manhattan ep = undefined


astarMisplacedTile :: QueueingFunction EightPuzzle
astarMisplacedTile = astar misplacedTile depth


astarManhattan :: QueueingFunction EightPuzzle
astarManhattan = astar manhattan depth


uniformCostEightPuzzle :: QueueingFunction EightPuzzle
uniformCostEightPuzzle = uniformCost depth



main :: IO ()
main = do
  args <- getEnv "foo"
  putStrLn "hello world"
