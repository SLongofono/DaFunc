{-
Stephen Longofono
Haskell Homework
Part 3 of 3, complete morse decryption program
April 24, 2016

For this assignment, we created a few simple Haskell functions to "decrypt" a morse code string.
The required signature decodeText::String->String takes in a space-delimited morse code message,
and converts it to alphanumeric characters.  Then, it parses a cipher shift and the message content
from the de-morsed string and reports the shifted message.

There are a number of other required signatures included to force us to into a specific style of
solution.  One unusual requirement was that we had to use a specific alphabet where 0 comes AFTER the
rest of the numbers, which made the whole shifting process very different.

-}
--for splitting on delimiter
import Data.List.Split 

--for char manipulation
import Data.Char

main = do
    let msg = "---.. -- ..... .- .... ----- --... .--- --... -.... ...-- ..-. -.... . --. .--- -.- --... -...."
    putStrLn("Original message: ")
    putStrLn(msg)
	putStrLn("Decoded: ")
    putStrLn(decodeText msg)
	
	
----------------------Part III Code----------------------------------
decodeText::String->String
decodeText input = decodeCipher (getMessage input) (getShift input)

getShift::String->Int
getShift input = getDecimal (sanitizeNum (fst (splitOnM (getAlpha input))))

getMessage::String->String
getMessage input = snd(splitOnM (getAlpha input))

getAlpha::String->String
getAlpha input = collapseIt (map fromMorseCode (tokenizeIt " " input)) 

--removes all non-number characters from a String
sanitizeNum::String->String
sanitizeNum [] = ""
sanitizeNum (x:[]) = if (isDigit x) then [x] else ""
sanitizeNum (x:xs) = if (isDigit x) then [x] ++ sanitizeNum xs else sanitizeNum xs

--returns the decimal value of a positive String of digits
getDecimal::String->Int->Int
getDecimal [] size = 0
getDecimal (x:[]) size = (digitToInt x) 
getDecimal (x:xs) size = ((pow 10 (size-1))*(digitToInt x)) + (getDecimal xs (size-1))

--simple integer powers
pow::Int->Int->Int
pow a 0 = 1
pow a b = (a* (pow a (b-1)))

--Generates a tuple from a string, the left half being everything up to an 'M' and the right everything beyond the 'M'
splitOnM::String->(String, String)
splitOnM [] = ([],[])
splitOnM xs = let (left, right) = splitAt (findM xs) xs in (left, (tail right))

--get first index of 'M' in a String
findM::[Char]->Int
findM xs
    |(length xs) == 0 = -9999999
    |(head xs)=='M' = 0
    | otherwise = 1 + (findM (tail xs))


---------------------Part II Code-----------------------------------

decodeCipher :: String -> Int -> String
decodeCipher xs x = forRealDecode (map toUpper xs) x

forRealDecode xs x
    | (length xs) == 0 = ""
    | (length xs) == 1 = [normalize x (head xs)]
    | otherwise = [normalize x (head xs)] ++ (forRealDecode (tail xs) x)
    where normalize :: Int -> Char -> Char
          normalize x ch 
              | (isDigit ch) =
				--map given char to 0th or 26th  (alpha vs digit) of validChars, then apply offest
				--numbers start at 48, uppercase letters at 65
				  if ((numIndex ch) + x > 35)
                  then (validChars!!(mod ((numIndex ch)+x) 36))
                  else (validChars!!((numIndex ch) + x) )
			  | (isAlpha ch) =
				  if (alphaIndex + x > 35)
                  then (validChars!!(mod (alphaIndex + x) 36))
                  else (validChars!!(alphaIndex + x))
			  | otherwise = ch
              where validChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
                    numIndex ch
					--extra special case for numbers.  Why?  Because our time is expendable, that's why.
					  | (ord ch) == 48 = 35	--0
						| (ord ch) == 49 = 26	--1
						| (ord ch) == 50 = 27	--2
						| (ord ch) == 51 = 28	--3
						| (ord ch) == 52 = 29	--4
						| (ord ch) == 53 = 30	--5
						| (ord ch) == 54 = 31	--6
						| (ord ch) == 55 = 32	--7
						| (ord ch) == 56 = 33	--8
						| otherwise = 34			--9 
                    alphaIndex = ((mod (ord ch) 65))

-------------------------Part I Code----------------------------

tokenizeIt :: String -> String -> [String]
tokenizeIt delim str = (splitOn delim str)

collapseIt :: [String] -> String
collapseIt xs
    |(length xs) == 0 = ""
    |(length xs) == 1 = (head xs)
    | otherwise = (head xs) ++ (collapseIt (tail xs))

fromMorseCode :: String -> String
fromMorseCode a
    |a==".-" = "A"
    |a=="-..." = "B"
    |a=="-.-." = "C"
    |a=="-.." = "D"
    |a=="." = "E"
    |a=="..-." = "F"
    |a=="--." = "G"
    |a=="...." = "H"
    |a==".." = "I"
    |a==".---" = "J"
    |a=="-.-" = "K"
    |a==".-.." = "L"
    |a=="--" = "M"
    |a=="-." = "N"
    |a=="---" = "O"
    |a==".--." = "P"
    |a=="--.-" = "Q"
    |a==".-." = "R"
    |a=="..." = "S"
    |a=="-" = "T"
    |a=="..-" = "U"
    |a=="...-" = "V"
    |a==".--" = "W"
    |a=="-..-" = "X"
    |a=="-.--" = "Y"
    |a=="--.." = "Z"
    |a==".----" = "1"
    |a=="..---" = "2"
    |a=="...--" = "3"
    |a=="....-" = "4"
    |a=="....." = "5"
    |a=="-...." = "6"
    |a=="--..." = "7"
    |a=="---.." = "8"
    |a=="----." = "9"
    |a=="-----" = "0"
    |otherwise = a
