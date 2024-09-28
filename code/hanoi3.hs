-- How to run this code
-- gchi
-- :load hanoi3

-- to understand the code mainly use ':type' and ':info'.

import Data.List (delete)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq  --This appears to just be the same as pythons import as, so we must prefix with Seq.
import Data.Foldable (toList)


-- type definitions
type Disk = Int
type Peg = [Disk]
type State = [Peg]   --so a state is [ [1..n][],[]]
type Move = (Int, Int)  -- Move disk from one peg to another
type Path = [Move]      -- Sequence of moves


-- Initial state: all disks are on the first peg
initialState :: Int -> State
initialState n = [reverse [1..n], [], [], []]

-- Goal state: all disks are on the target peg (let's say the last peg)
goalState :: Int -> State
goalState n = [[], [], [], reverse [1..n]]

-- Check if a move is valid: A larger disk cannot be placed on a smaller disk.
isValidMove :: State -> Move -> Bool
isValidMove state (from, to) =
  not (null (state !! from)) &&    --there must be a someting on the peg we are moving form (!! is li [-] in other languages - it seems that 'null' is from the 
                                  --typeclass foldable and simply checks (in the list/array impelemnetation) that the list is not empty
                                  -- this was found out by usign ':type' and ':info'.
  (null (state !! to) || head (state !! from) < head (state !! to))

applyMove :: State -> Move -> State
applyMove state (from, to)
    | null (state !! from) = state  -- If the 'from' peg is empty, return the state unchanged
    | otherwise =
        let fromPeg = state !! from  -- this appears to be the syntax for getting ith element form list, i.e. a!!i = a[i] in python.
            toPeg = state !! to
            (disk, restFrom) = (last fromPeg, init fromPeg)  -- Take the last element from 'from'. Last appears to take last, init (=inital) appears to drop last.So opposite to head and tail.
        in [ if i == from then restFrom  -- Update the 'from' peg (remove the last disk)
            else if i == to then toPeg ++ [disk]  -- Update the 'to' peg (add the last disk)
            else peg  -- Other pegs remain unchanged
            | (i, peg) <- zip [0..] state]--this will just append the number to the elements in the list state such that [[1,2],[2]]->[(0,[1,2]),(1,[2])]
                                          --Haskell do have very nice list-syntax: [i>4 | i<-[0..10]].   
            
-- Generate all possible valid moves for a given state
generateMoves :: State -> [Move]
generateMoves state = [(from, to) | from <- [0..3], to <- [0..3], from /= to, isValidMove state (from, to)]

-- Functional BFS to find the shortest path from the initial state to the goal state
-- Note that we are here using Seq and Set, mainly for performance reasons.
bfs :: State -> State -> Path
bfs start goal = bfs' (Seq.singleton (start, [])) Set.empty
  where
    bfs' :: Seq.Seq (State, Path) -> Set.Set State -> Path   --Set.Set doesnt seem to have a constructor. Below we seem to build up the set of visited nodes, 
                                                              -- by adding them manually to an emptu set.
    bfs' Seq.Empty _ = error "No solution found"  -- Shouldn't happen for valid inputs
    bfs' ((current, path) Seq.:<| queue) visited   -- The syntax Seq.:<| appears to identical to ':'. Main difference appears to be that Seq.:<| works on squences, and have better performance.
      | current == goal  = path  -- Goal reached
      | current `Set.member` visited = bfs' queue visited  -- Skip already visited states - Clearly the Set.member will mere check that we are not already in the list.
      | otherwise = bfs' (queue Seq.>< Seq.fromList newStates) (Set.insert current visited) --Note that at this point we are inserting into out queue. 
                                                                --Since this is a functional queue so we are acutally createing a new one, but the vast-vast majority 
                                                                --of the queue is resued, only a few references are updated, and the pointer to head. Hence the overhead here is neglible.
      where
        possibleMoves = generateMoves current
        newStates = [(applyMove current move, path ++ [move]) 
                     | move <- possibleMoves, 
                       let nextState = applyMove current move,
                       nextState `Set.notMember` visited]

-- Solve the problem for a given number of disks
solveHanoi :: Int -> Path
solveHanoi n = bfs (initialState n) (goalState n)

-- Print the solution
printSolution :: Path -> IO ()
printSolution = mapM_ print

-- Example usage:
main :: IO ()
main = do
  let n = 5  -- Number of disks
  let solution = solveHanoi n
  printSolution solution


--test code
testInitialGoalState :: IO ()
testInitialGoalState = do
  let n = 3
  print $ initialState n == [[3,2,1], [], [], []]  -- Should print True
  print $ goalState n == [[], [], [], [3,2,1]]    -- Should print 
  
testIsValidMove :: IO ()
testIsValidMove = do
    let state = [[3, 2, 1], [], [], []]  -- All disks on peg 0
    print $ isValidMove state (0, 1) == True  -- Valid move from peg 0 to peg 1
    print $ isValidMove state (0, 2) == True  -- Valid move from peg 0 to peg 2
    print $ isValidMove state (1, 0) == False -- Invalid move (peg 1 is empty)
    print $ isValidMove state (0, 0) == False -- Invalid move (same peg)

testApplyMove :: IO ()
testApplyMove = do
    let state = [[3, 2, 1], [], [], []]
    let expectedState = [[3, 2], [1], [], []]
    print $ applyMove state (0, 1) == expectedState  -- Moving top disk from peg 0 to peg 1

testGenerateMoves :: IO ()
testGenerateMoves = do
    let state = [[3, 2, 1], [], [], []]
    let expectedMoves = [(0, 1), (0, 2), (0, 3)]
    print $ generateMoves state == expectedMoves  -- Should generate valid moves from peg 0

testBFS1 :: IO ()
testBFS1 = do
    let n = 1
    let solution = solveHanoi n
    let expectedSolution = [(0, 3)]  -- One move: from peg 0 to peg 3
    print $ solution == expectedSolution

test::IO()
test = do
    testInitialGoalState
    testIsValidMove
    testApplyMove
    testGenerateMoves
    testBFS1   
    
    