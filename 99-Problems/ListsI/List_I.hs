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
	putStrLn( show( flatten_lst (List 1,[2,[3,4],5]) ) )



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

--Problem 4 - Reverse a list
flipMode::[a]->[a]
flipMode [] = []
flipMode (x:[]) = [x]
flipMode xs = [(last xs)] ++ (flipMode (init xs))

pally::(Eq a)=>[a]->Bool
pally [] = False
pally (x:[]) = True
pally (x:y:[]) = x == y
pally xs = ((head xs) == (last xs)) && (pally (tail (init xs)))


flatten_lst::NestedListType a->[a]
flatten_lst (Elem x) = [x]
flatten_lst (List []) = []
flatten_lst (List (x:xs)) = flatten_lst x ++ flatten_lst (List xs)
