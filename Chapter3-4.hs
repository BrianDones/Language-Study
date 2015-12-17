{-
 - Brian Dones
 - Professor Johnson
 - Language Study
 - 1/29/14

 - For this exercise, we are dealing with a type for colours of the rainbow
 - The typeclass is defined here, and note its English spelling.
 - For more information on how this is done, look ahead to:
 - http://learnyouahaskell.com/making-our-own-types-and-typeclasses
 -
 - Have a play with the Colour in ghci, try the succ and pred functions and so on.
 -}
data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet | Teal
    deriving (Eq, Ord, Show, Bounded, Enum)   

{-
 - Again, you should be able to write these functions in one line, 
 - using the information from the chapter http://learnyouahaskell.com/types-and-typeclasses
 - and the chapter before
 -}
 
-- This generates a list given the data type "Colour"

-- Having this function with a range that isn't given an end value
-- lets the user add in extra "Colours" and the list will include the
-- new values as oppose to having to change the end value in the fucntions
-- definition.

generatedList = [(minBound::Colour) .. (maxBound::Colour)]

{-
 - The Colour typeclass is of type Ord
 - What is the "first" (or least) colour
 -}

firstColour = head (generatedList)

-- List the colours in reverse order
reverseColourOrder = [(maxBound::Colour), (pred maxBound::Colour) .. (minBound::Colour)]

{-
 - Mix two colours together, to produce the average value of the two.
 - Example: paintMix Orange Green = Yellow
 - If necessary, favour the "higher" value when computing the average.
 - For example: paintMix Green Violet = Indigo
 - Hint: Integer division can be performed with the quot function: quot 7 2 = 3
 -}
 
-- This function takes in a color value and returns a List containing an integer value for the color
findIt c = [(x + 1)| x <- [0 .. ((length generatedList)-1)], c == generatedList !! x]

-- This function takes the list from the findIt function and returns the integer value for the color
printIt :: [Int] -> Int
printIt [1] = 1
printIt [2] = 2
printIt [3] = 3
printIt [4] = 4
printIt [5] = 5
printIt [6] = 6
printIt [7] = 7


paintMix c1 c2 = generatedList !! (((quot ((printIt(findIt(c1))) + (printIt(findIt(c2)))) 2) + rem ((printIt(findIt(c1))) + (printIt(findIt(c2)))) 2)-1)

-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9

--Solution using Guards
englishDigit :: Int -> String
englishDigit x 
	| x < 0 = "unknown"
	| x > 9 = "unknown"
	| otherwise = show x
	
--Solution using pattern matching
englishDigit' :: Int -> String
englishDigit' 0 = show 0
englishDigit' 1 = show 1
englishDigit' 2 = show 2
englishDigit' 3 = show 3
englishDigit' 4 = show 4
englishDigit' 5 = show 5
englishDigit' 6 = show 6
englishDigit' 7 = show 7
englishDigit' 8 = show 8
englishDigit' 9 = show 9
englishDigit' x = "Unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero

--Solution using Guards
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x,y)
	| fst (x,y) == 0 = error "dividing zero by a number: Infinity"
	| snd (x,y) == 0 = error "division by zero: undefined"
	| otherwise 	 = (fst (x,y) / snd (x,y))

--Solution using pattern matching
divTuple' :: (Eq a, Fractional a) => (a, a) -> a
divTuple' (_, 0) = error "division by zero: undefined"
divTuple' (0, _) = error "dividing zero by a number: Infinity"
divTuple' (x,y) = (fst (x,y) / snd (x,y))

-- if the first three numbers in a list are all zero, return True

--Solution using Guards
threeZeroList :: [Int] -> Bool
threeZeroList list 
	| take 3 list == [0,0,0] = True
	| otherwise = False
	
--Solution using pattern matching
threeZeroList' :: [Int] -> Bool
threeZeroList' [0,0,0] = True
threeZeroList' [0,0,0,_] = True
threeZeroList' list = False
