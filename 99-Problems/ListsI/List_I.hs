data NestedListType a = Elem a | List [NestedListType a]

main = do

	let test1 = [1,2,3,4,5,6]
	let test2 = "Apples"
	let test3 = "Stephenehpets"
	let test4 = "racecar"
	let test5 = [1,2,3,3,2,1]

	putStrLn( "Tests" )

	putStrLn( "\n\nProblem 1" )
	putStrLn( show( myLast test1  ) )
	putStrLn( show( myLast test2  ) )

	putStrLn( "\n\nProblem 2" )
	putStrLn( show( secondToLast test1  ) )
	putStrLn( show( secondToLast test2  ) )

	putStrLn( "\n\nProblem 3" )
	putStrLn( show( nthEntry test1 3 ) )
	putStrLn( show( nthEntry test2 0 ) )

	putStrLn( "\n\nProblem 4" )
	putStrLn( show( len test1  ) )
	putStrLn( show( len test2  ) )


	putStrLn( "\n\nProblem 5" )
	putStrLn( show( flipMode test1  ) )
	putStrLn( show( flipMode test3  ) )


	putStrLn( "\n\nProblem 6" )
	putStrLn( show( pally test2  ) )
	putStrLn( show( pally test3  ) )
	putStrLn( show( pally test4  ) )
	putStrLn( show( pally test5  ) )


	putStrLn( "\n\nProblem 7" )
	putStrLn( show( flatten_lst (List [ List [Elem 1, List[Elem 2, List [Elem 3, Elem 4], Elem 5]]] ) ) )


	putStrLn( "\n\nProblem 8" )
	putStrLn( show( compress [1,1,1,2,2,2,3,3,3,1,2,3,1,1,2,3,1,2,2,3,3,1,1,3] ) )
	putStrLn( show( compress [1,1,1] ) )
	putStrLn( show( compress [3] ) )


	putStrLn( "\n\nProblem 9" )
	putStrLn( show( groupInstance [1,1,1,1,2,3,3,1,1,4,5,5,5] 1 ) )
	putStrLn( show( groupInstance [2,2,2,3,1,2,1,3,4,5,2,2,2,4,4,5] 2 ) )

--Problem 1 - Find the last element of a list
myLast::[a]->a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

--Problem 2 - Find the next to last element of a list
secondToLast::[a]->a
secondToLast (x:y:[]) = x
secondToLast (x:xs) = secondToLast xs

--Problem 3 - Find the nth element of a list
nthEntry::[a]->Int->a
nthEntry xs 0 = head xs
nthEntry xs n = myLast (take n xs)

--Problem 4 - Count elements in a list
len::[a]->Int
len [] = 0
len (x:[]) = 1
len (x:xs) = 1 + (len xs)

--Problem 5 - Reverse a list
flipMode::[a]->[a]
flipMode [] = []
flipMode (x:[]) = [x]
flipMode xs = [(last xs)] ++ (flipMode (init xs))

--Problem 6 - Determine if a list is a palindrome
pally::(Eq a)=>[a]->Bool
pally [] = False
pally (x:[]) = True
pally (x:y:[]) = x == y
pally xs = ((head xs) == (last xs)) && (pally (tail (init xs)))

--Problem 7 - Flatten a nested list structure
flatten_lst::NestedListType a->[a]
flatten_lst (Elem x) = [x]
flatten_lst (List []) = []
flatten_lst (List (x:xs)) = flatten_lst x ++ flatten_lst (List xs)

--Problem 8 - Eliminate consecutive repeated elements in a list
compress::(Eq a)=>[a]->[a]
compress xs
    |(length xs) < 2 = xs
    |(head xs) == (head (tail xs)) = (compress (tail xs))
    |otherwise = [(head xs)] ++ (compress (tail xs))

--Problem 9 - Pack consecutive repeated elements into sub-lists
{-
compress to get a list of chars to test with
curry with the first elem of that list and groupInstance
remove the length of the resultant list from the original
curry with the next element of the compressed list
aggregate the returns of groupInstance
-}
pack::(Eq a)=>[a]->[a]
pack xs = []


--Helper for #9 - returns a list of matching consecutive characters from the front of a list
groupInstance::(Eq a)=>[a]->a->[a]
groupInstance xs c
    |(length xs) == 0 = []
    |(head xs) == c = [(head xs)] ++ (groupInstance c (tail xs))
    |otherwise = []
