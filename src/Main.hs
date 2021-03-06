module Main where

import           Control.Monad        (unless)
import           Data.List            (foldl', sortBy)
import qualified Data.Matrix          as M (fromList)
import           Data.Maybe           (catMaybes)
import           Data.Ord             (comparing)
import           Data.PQueue.Prio.Min (MinPQueue, insert, minView, singleton,
                                       size)
import           Data.Vector          (Vector, (!), (//))
import qualified Data.Vector          as V (concatMap, elemIndex, filter,
                                            findIndex, fromList, indexed,
                                            length, map, slice, toList, zipWith)
import           System.IO            (hFlush, stdout)

{-  We'll start by defining a few abstract building blocks for pieces of the general
    search algorithm. Allowing us to generalize to any NPuzzle, or any other
    search problem, as long as we can implement this interface

    We'll then define concrete datatypes for the eight puzzle problem
    and connect them to the abstract interface.

    In order to provide a generic interface for other search problem, we'll be
    making use of Haskell's hefty type syStatetem. See the follwoing link if you get
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
applyAll functions value =
    case functions of
        [] -> []
        (firstFunction:rest) -> firstFunction value : applyAll rest value
-- apply each function in a list of functions to some value, returning a list of results
-- 'state' here is a value-level function argument, not a type variable


{-  A queueing function takes the current queue and the expanded states, and
    inserts them into the queue with some cost. The cost determines the dequeing
    order, which determines the nature of the search algorithm. For our class of
    problems, we'll use a min priority queue that will dequeue its entries in
    order of increasing 'Cost'.
-}

type Queue state = MinPQueue Cost state
-- forces us to use 'Int' valued Cost for all problems

type QueueingFunction state = Queue state -> [state] -> Queue state

{- We can finally write the general search algorithm, which takes anything of type 'state'
   implementing the 'Problem' interface, plus a queueing function with which to
   order the insertion into the search queue.

   We add two unfortunate parameters, the g and h functions, which are necessary
   at this stage to print the associated costs on each iteration.
   Then, following the psuedocode, we pass in an initial problem state and a queueing
   function to order node insertion. We return an 'Either' a 'String' to denote
   that the operation might fail, producing an error 'String' or the correct
   solution state and all the attendant statistics. There are more idiomatic
   wayState to maintain the trace, queue size and number of nodes expanded, but I
   decided to stick to argument passing to avoid syntactic confusion.

   Haskell doesn't have an immediately obvious way to express while loops,
   so I use the recursive 'go' function to implement iteration. The 'go'
   function also uses the 'do' syntax sugar to look more imperative
-}

-- Possibly replace trace string with IO embedding
generalSearch :: (Show state, Problem state) => state
                                             -> (state -> Cost)
                                             -> (state -> Cost)
                                             -> QueueingFunction state
                                             -> Either String (state, Int, Int, String)
generalSearch initialState g h =
    go (singleton 0 initialState) 0 1 "Expanding State... \n" -- start with initial cost 0, 'singleton' initializes queue
        where go nodes nodeCount maxSize trace enqueue = do
                (node, queue) <- case minView nodes of
                                    Nothing   -> Left "No Solution"
                                    Just view -> Right view
                if isGoal node
                  then return (node, nodeCount, maxSize, trace ++ show node)
                  else go (enqueue queue (expand node))
                          (nodeCount + length (expand node))
                          (max maxSize (size nodes))
                          (trace `seq` trace ++ prettyPrint node g h) -- force the 'trace' accumulator
                          enqueue
-- Using "Maybe a = Just a | Nothing" is the haskelly way of checking for null
-- in this case, we minView to return either 'Just' the dequeued node and the new
-- queue, or 'Nothing', telling us that the queue was empty

prettyPrint :: (Show state, Problem state) => state
                                           -> (state -> Cost) -- the depth
                                           -> (state -> Cost) -- the heuristic
                                           -> String
prettyPrint n g h =
    "The best state to expand with g(n) = "
 ++ show (g n)
 ++ " and h(n) = "
 ++ show (h n)
 ++ " is...\n"
 ++ show n
 ++ "Expanding this node...\n\n"



{- Since every one of our search procedures is a derivative of A*, we can define a
   general version first, then pass the g and h functions as arguments.

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


-- Using the matrix package allows us to keep the inverted representation while
-- pretty printing something close to the actual board structure
instance Show EightPuzzle where
    show = show
         . M.fromList 3 3
         . map fst
         . sortBy (comparing snd)
         . V.toList
         . V.indexed
         . board



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
-- the (Just neighbor) pattern match will throw an exception if 'elemIndex' returns
-- 'Nothing'


-- swap the element at index1 with the element at index2
swap :: Vector a -> Int -> Int -> Vector a
swap vec index1 index2 =
    vec //
        [ (index1, vec ! index2)
        , (index2, vec ! index1)
        ]



{-  With the general function in place, we can specialize it for each direction
-}

moveBlankLeft :: Operator EightPuzzle
moveBlankLeft = moveBlank (\x -> x - 1) $ (> 0) . (`mod` 3)


moveBlankRight :: Operator EightPuzzle
moveBlankRight = moveBlank (+ 1) $ (< 2) . (`mod` 3)


moveBlankUp :: Operator EightPuzzle
moveBlankUp = moveBlank (\x -> x - 3) (> 2)


moveBlankDown :: Operator EightPuzzle
moveBlankDown = moveBlank (+ 3) (< 6)


{-  Here's the 'instance' declaration we talked about earlier, where we finally
    specify how 'EightPuzzle' implements the 'Problem' interface.

    Remember that 'expand' is already defined, so we have no need to define it
    here.
-}


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
--        = board (makePuzzle [1,2,3,4,5,6,7,8,0])


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


manhattan16 :: EightPuzzle
manhattan16 = makePuzzle [8,4,3,5,1,7,6,2,0]

{- Finally, we can specialize the search algorithm that we defined earlier by
   plugging in the appropriate heuristics and depth functions

   Pardon the rather terse Haskell being displayed here ... it might help to
   read the definition from right to left, and to think of each (.) as applying
   the next function on its left to the result so far.
-}

-- extract the board from the puzzle, compare it against the goal, and count up the tiles (excluding the blank) that don't match
misplacedTile :: EightPuzzle -> Cost
misplacedTile =
    V.length . V.filter (== False) . V.slice 1 8 . V.zipWith (==) goalBoard . board


-- implemented by referencing https://heuristicswiki.wikispaces.com/Manhattan+Distance
-- apply toXy to the argument and the goal, calculate individual coordinate distances (excluding the blank), sum them up
manhattan :: EightPuzzle -> Cost
manhattan =
    sum . V.slice 1 8 . V.zipWith distance (V.map toXy goalBoard) . V.map toXy . board


-- convert position into an (x,y) coordinate pair
toXy :: Position -> (Int,Int)
toXy pos = (pos `div` 3, pos `mod` 3)


-- sum the differences along the x and y axes
distance :: (Int,Int) -> (Int,Int) -> Cost
distance (xState,yState) (xGoal,yGoal) =
    abs (xState - xGoal) + abs (yState - yGoal)


astarMisplacedTile :: QueueingFunction EightPuzzle
astarMisplacedTile = astar misplacedTile depth


astarManhattan :: QueueingFunction EightPuzzle
astarManhattan = astar manhattan depth


uniformCostEightPuzzle :: QueueingFunction EightPuzzle
uniformCostEightPuzzle = uniformCost depth


-- I was all ready to dig into Haskell memory profiling when I realized that
-- the puzzle I was testing on was unsolvable. Here's a simple check I found
-- at http://ldc.usb.ve/~gpalma/ci2693sd08/puzzleFactible.txt
isSolvable :: EightPuzzle -> Bool
isSolvable (EightPuzzle _ board)=
    let board' = V.slice 1 8 board
    in
        even
        $ V.length
        $ V.filter (== True)
        $ V.concatMap (\(x,lst) -> V.map (x <) lst )
        $ V.fromList [(board' ! i, V.slice 0 i board') | i <- [0..(V.length board' - 1)]]


main :: IO ()
main = do
  putStrLn "Nick Lawler's Amazing 8 Puzzle!!"
  putStrLn "Hit 1 to run the benchmark or 2 to enter an arbitrary puzzle"
  choice <- getLine
  case choice of
      "1" -> benchmark
      "2" -> arbitraryPuzzle
      _   -> do putStrLn "Listen to Instructions! \n"
                main


benchmark :: IO ()
benchmark = undefined

-- TODO, choose heuristic with algorithm
arbitraryPuzzle :: IO ()
arbitraryPuzzle = do
    putStrLn "Enter rows using ' ' as a separator, use '0' to represent the blank"
    putStr "Enter the first row:  "
    hFlush stdout
    first <- getLine
    putStr "Enter the second row: "
    hFlush stdout
    second <- getLine
    putStr "Enter the thrid row:  "
    hFlush stdout
    third <- getLine
    let puzzle = makePuzzle (map read $ concatMap words [first,second,third])
    unless (isSolvable puzzle) $
        do putStrLn "Puzzle isn't solveable, try again"
           arbitraryPuzzle
    algorithm <- chooseAlgo
    case generalSearch puzzle depth manhattan algorithm of
        Left err -> putStrLn err
        Right (state, numNodes, maxSize, trace) -> do
            putStrLn trace
            putStrLn "\nGoal!!!\n"
            putStrLn $ "To solve this problem the search algorithm expanded a total of " ++ show numNodes ++ " nodes"
            putStrLn $ "The maximum number of nodes in the queue at any one time was " ++ show maxSize
            putStrLn $ "The depth of the goal node was " ++ show (depth state)
    return ()


chooseAlgo :: IO (QueueingFunction EightPuzzle)
chooseAlgo = do
    putStrLn "Enter 1 for Uniform Cost Search"
    putStrLn "Enter 2 for A* Misplaced Tile"
    putStrLn "Enter 3 for A* Manhattan Distance"
    choice <- getLine
    case choice of
        "1" -> return uniformCostEightPuzzle
        "2" -> return astarMisplacedTile
        "3" -> return astarManhattan
        _   -> do putStrLn "1, 2, or 3, not that hard \n"
                  chooseAlgo
