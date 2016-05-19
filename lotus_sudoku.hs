{-
===============================================================================
Luke Dercher & Stephen Longofono
EECS 368
Project # 2 - Lotus Sudoku Solver in Haskell
April 30, 2016
MIT License

===============================================================================

Lotus Sudoku Puzzle Solver

Problem Description: Fill in all cells such that:

	1. Every arc (left and right) contains the numbers from 1 to 7
	2. Every ring of colors contains the numbers from 1 to 7
	3. No number can be repeated in any ring or arc.

	Indexing is determined by the rings, counting from position 0 through 48 inclusive, beginning
	from the topmost point and starting at the first entry at or to the right of center in each successive ring.

	The testing suite will assume the following signature:

	lotusSolver::[Int]->[Int]
-}

--Enter your test code here


{-----------------------------------------------------------------------
-----------------------------Solution Code------------------------------
------------------------------------------------------------------------}
{-
This method is the required signature for the testing suite.
Note that here we use the findNext helper, since we actually do want to check  every element 
in the board passed in.

@input - An integer list representing the board to be solved
@process - hands off the responsibility of solving the puzzle, and eagerly awaits the chance
	to present the result as its own accomplishment
@return - An integer list representing the solved board, or an empty list if no solution was found

-}

lotusSolver::[Int]->[Int]
lotusSolver xs = solver4 xs (findNext xs)
	
{-
This is the main solver algorithm. It was conceived by our instructor, and shared with us
after we had struggled for an acceptable amount of time with our own naive attempts.  We 
had already implemented the helper methods for our previous attempts.

The process is as follows:

If index is 49, we have reached the end of a branch, and the board passed in 
is returned unchanged.

Otherwise, a number is placed into the board at the indicated index.
if this placement is valid, the resultant board, and the next empty entry in that
board, are passed into a recursive call to solver4.  This attempts to solve that board, 
and if the return is also a valid board, then that board is a solution.  This process is
repeated for each of the numbers 1-7 (the only valid numbers for any given position).

If none of the valid numbers resulted in a solution, then the board is not solvable and an
empty list is returned.  This becomes another terminating condition for each branch of recursion.

It is important to note how this uses lazy evaluation and guards to provide structure to the evaluation.
Each of the  'try' expressions is only evaluated when it needs to be for validPlace, which allows us to
follow our branches in a procedural style and make effective use of the checking helpers to rule out sub-branches.

For example. at the second guard, first n1 is generated, then it is checked for validity, and only then is the second
clause of the conditional evaluated.  In this arrangement, a branch will never be followed if the resultant placement is invalid.

The where clause allows us to use the same computation more than once, as demonstrated in the 'try' results.  If the 
recursive call happens to return a valid board, we already have the result in hand and it need not be evaluated again.

@inputs - A list of integers, representing the current state of the lotus board at hand
@process - Recovers solutions for the board, assuming the board is of the proper size (49) and type 
@return - a solution to the puzzle as a list of integers, or an empty list if none was found
-}

solver4::[Int]->Int->[Int]
solver4 board index
    | (index == 49) = board
    | (validPlace n1) && (validPlace try1) = try1
    | (validPlace n2) && (validPlace try2) = try2
    | (validPlace n3) && (validPlace try3) = try3
    | (validPlace n4) && (validPlace try4) = try4
    | (validPlace n5) && (validPlace try5) = try5
    | (validPlace n6) && (validPlace try6) = try6
    | (validPlace n7) && (validPlace try7) = try7
    | otherwise = []
        where n1 = placeNum board index 1
	            n2 = placeNum board index 2
              n3 = placeNum board index 3
	            n4 = placeNum board index 4
	            n5 = placeNum board index 5
              n6 = placeNum board index 6
              n7 = placeNum board index 7
              try1 = solver4 n1 (newIndex n1 index)
              try2 = solver4 n2 (newIndex n2 index)
              try3 = solver4 n3 (newIndex n3 index)
              try4 = solver4 n4 (newIndex n4 index)
              try5 = solver4 n5 (newIndex n5 index)
              try6 = solver4 n6 (newIndex n6 index)
              try7 = solver4 n7 (newIndex n7 index)

{-
This helper is used to insert a number into a list in place of the index passed in.
It uses a pretty interesting feature of Haskell in that we can assign a name to 
constituent parts of a function's return and operate on it without having to decompose
the tuple manually
@inputs - A list of integers representing the state of a board, an integer representing an
	index in the board, and an integer to replace the value at the given index
@process - replaces the value at the index of the first integer argument with the value of
	the second integer argument
@return - A list of integers, representing the board with the number placed at the indicated
	position
-}
placeNum::[Int]->Int->Int->[Int]
placeNum xs index num = (let (left, right) = splitAt index xs in left ++ [num] ++ (tail right))

{-
This helper is used to find the first non-positive index of an integer array,
indicating the position where the next number should be placed.

@input - An integer list representing the current state of the board
@process - Identifies the first entry with a value less than 1 
@return - An integer representing the index of the first entry with a value less than 1,
	or -1 if no such entry exists
-}
findNext::[Int]->Int
findNext [] = -1
findNext xs = if (head xs) < 1 then 0 else 1 + (findNext (tail xs))

{-
This helper behaves similarly to the findNext method, except that it only
checks indices beyond the one passed in.  Haskell lists are represented as
singly linked lists. This makes operating on indices slightly more efficient by
only checking the indices we care about.

@input - An integer list representing the current state of the board, and an integer representing
	what index in the list to begin searching from
@process - beginning at the integer index passed in, this function locates the first entry beyond
	that index with a value less than 1
@return - An integer representing the first index beyond the index passed in where the value is
	less than 1, or -1 if no such entry exists
-}
entryBeyond::[Int]->Int->Int
entryBeyond xs n
    |(n+1)>48 = -1  --case no unfilled beyond original n
    |(xs!!(n+1))<1 = (n+1) --case found one at the next index
    | otherwise = entryBeyond xs (n+1) --look ahead one further

{-
This helper is used to handle a list with no more empty indexes to 
be filled, in which case it returns 49, the terminating condition for the 
recursion in solver4.

@input - An integer list representing the current state of the board, and an integer
	representing what index in the list to begin searching from
@process - calls entrybeyond using the index and board passed in, and handles a negative return
@return - returns the first entry beyond the given index which has a value less than 1,
	or returns 49, the terminating condition for the solver4 method, if the entryBeyond
	method indicates that there are no such entries.
-}
newIndex::[Int]->Int->Int
newIndex xs index
    |(entryBeyond xs index) < 0 = 49
    | otherwise = entryBeyond xs index

{-
This helper method checks for duplicates of positive numbers, which
offers a way to check if a placed number is valid before a given board is solved.
In tandem with the noEmpties helper, it is also used to determine if a board
is solved.  Process is as follows:
filter out negative numbers
create a set from this list
if set length is smaller than list length, then we have duplicates

@input - An integer list representing a ring or arc in the current board
@process - determines if there are duplicate non-negative entries in the list
@return - True if no duplicates were found, false otherwise

-}
noDupes::[Int]->Bool
noDupes xs = ((length (filter (>0) xs)) == (length xSet))
    where xSet = filter (>0) (unique xs)
{-
This helper is used to avoid having to import the Set library, since it interferes
with the testing suite for grading.  The method strips out duplicate entries with the
filter method.

Big thanks rosettacode.org for this method, although I suspect one could do better than
n^2 time here.  Retrieved from http://www.rosettacode.org/wiki/Remove_Duplicate_Elements#Haskell
April 4, 2016

@input - A list of well-ordered type
@process - removes all but one of every unique value in the list
@return - a list containing only the unique values from the list passed in

-}
unique::Eq a=> [a] -> [a]
unique [] = []
unique (x:xs) = x : unique (filter (x/=) xs)
    
{-
This helper method checks for unfilled entries by validating each element
of the list passed in.  Each entry is valid if it is in the set of integers [1,7]
and is invalid otherwise.	

@input - An integer list representing a ring or arc in the current board
@process - determines if all the entries are in the set [1,7]
@return - True if all the entries are in the set [1,7], false otherwise

-}
noEmpties::[Int]->Bool
noEmpties [] = True
noEmpties (x:[]) = (x>0)
noEmpties xs = ((head xs)>0) &&  ((head xs)<8) && noEmpties (tail xs)

{-
This helper combines the noDupes and noEmpties methods to determine
if a list can be considered complete.

@input - An integer list representing a ring or arc in the current board
@process - determines if the list is complete, defined as having no duplicate
	entries, and all entries as memebers of the set [1,7]
@return - True if the board is complete as defined above, false otherwise
-}
isComplete::[Int]->Bool
isComplete xs = (noDupes xs) && (noEmpties xs)

{-
This helper gathers all the rings, left arcs, and right arcs for the given board, and 
checks that all are valid.  This helper is used after placing a number in an entry to 
determine whether or not to pursue the resulting board.
Note, this does not expressly determine if a board is solved, but if this returns true
and all entries are filled, then the board is by definition solved.  Process:
populate an array containing all rings, left arcs, and right arcs from a board
filter out any that have duplicates and compare the resulting list to the original
if any have been removed (lengths don't match) then the placement was invalid.

@input - An integer list representing the current state of the board
@process - Assembles a list of integer lists representing all the rings and arcs for the board passed in.
	Each of these is checked for duplicate entries, the goal being that a valid board has no duplicates.
@return - True if no rings or arcs in the board passed in contain duplicate entries, False otherwise.

-}
validPlace::[Int]->Bool
validPlace xs
    |(length xs) == 0 = False
    |otherwise  = (length checklist) == (length (filter (noDupes) checklist))
        where checklist = (getRightArcs xs 6) ++ (getLeftArcs xs 6) ++ (getRings xs 6)

{-
These three helper methods retrieve the left arcs, right arcs, and rings for a given board.  There is almost certainly a pattern we 
could have used to retrieve them, but in terms of performance, the result would be the same.  Given a list, Haskell will traverse
it as a singly-linked list, regardless of how one finds the index.  The Vector library offers an improved access time, which could 
be an improvement for a future implementation.
For each of the following three methods, we manually mapped the rings and arcs to the numbering scheme given in the rubric.

@input - An integer list representing the current state of the board
@process - For each of the three methods below, a list of integerlists is assembled from the board passed in, representing all of the
	left arcs, right arcs, OR rings.  These follow a pattern established in the problem rubric.
@return - a list of integer lists representing either all the left arcs, all the right arcs, or all the rings for the board passed in.

-}
getRightArcs::[Int]->Int->[[Int]]
getRightArcs board 0 = [[(board!!0),(board!!7),(board!!15),(board!!22),(board!!30),(board!!37),(board!!45)]]
getRightArcs board 1 = [[(board!!1),(board!!8),(board!!16),(board!!23),(board!!31),(board!!38),(board!!46)]] ++ (getRightArcs board 0)
getRightArcs board 2 = [[(board!!2),(board!!9),(board!!17),(board!!24),(board!!32),(board!!39),(board!!47)]] ++ (getRightArcs board 1)
getRightArcs board 3 = [[(board!!3),(board!!10),(board!!18),(board!!25),(board!!33),(board!!40),(board!!48)]] ++ (getRightArcs board 2)
getRightArcs board 4 = [[(board!!4),(board!!11),(board!!19),(board!!26),(board!!34),(board!!41),(board!!42)]] ++ (getRightArcs board 3)
getRightArcs board 5 = [[(board!!5),(board!!12),(board!!20),(board!!27),(board!!28),(board!!35),(board!!43)]] ++ (getRightArcs board 4)
getRightArcs board 6 = [[(board!!6),(board!!13),(board!!14),(board!!21),(board!!29),(board!!36),(board!!44)]] ++ (getRightArcs board 5)

getLeftArcs::[Int]->Int->[[Int]]
getLeftArcs board 0 = [[(board!!0),(board!!13),(board!!20),(board!!26),(board!!33),(board!!39),(board!!46)]]
getLeftArcs board 1 = [[(board!!1),(board!!7),(board!!14),(board!!27),(board!!34),(board!!40),(board!!47)]] ++ (getLeftArcs board 0)
getLeftArcs board 2 = [[(board!!2),(board!!8),(board!!15),(board!!21),(board!!28),(board!!41),(board!!48)]] ++ (getLeftArcs board 1)
getLeftArcs board 3 = [[(board!!3),(board!!9),(board!!16),(board!!22),(board!!29),(board!!35),(board!!42)]] ++ (getLeftArcs board 2)
getLeftArcs board 4 = [[(board!!4),(board!!10),(board!!17),(board!!23),(board!!30),(board!!36),(board!!43)]] ++ (getLeftArcs board 3)
getLeftArcs board 5 = [[(board!!5),(board!!11),(board!!18),(board!!24),(board!!31),(board!!37),(board!!44)]] ++ (getLeftArcs board 4)
getLeftArcs board 6 = [[(board!!6),(board!!12),(board!!19),(board!!25),(board!!32),(board!!38),(board!!45)]] ++ (getLeftArcs board 5)

getRings::[Int]->Int->[[Int]]
getRings board 0 = [[(board!!0),(board!!1),(board!!2),(board!!3),(board!!4),(board!!5),(board!!6)]]
getRings board 1 = [[(board!!7),(board!!8),(board!!9),(board!!10),(board!!11),(board!!12),(board!!13)]] ++ (getRings board 0)
getRings board 2 = [[(board!!14),(board!!15),(board!!16),(board!!17),(board!!18),(board!!19),(board!!20)]] ++ (getRings board 1)
getRings board 3 = [[(board!!21),(board!!22),(board!!23),(board!!24),(board!!25),(board!!26),(board!!27)]] ++ (getRings board 2)
getRings board 4 = [[(board!!28),(board!!29),(board!!30),(board!!31),(board!!32),(board!!33),(board!!34)]] ++ (getRings board 3)
getRings board 5 = [[(board!!35),(board!!36),(board!!37),(board!!38),(board!!39),(board!!40),(board!!41)]] ++ (getRings board 4)
getRings board 6 = [[(board!!42),(board!!43),(board!!44),(board!!45),(board!!46),(board!!47),(board!!48)]] ++ (getRings board 5)
