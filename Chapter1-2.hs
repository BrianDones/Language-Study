{-
 - Brian Dones
 - Professor Johnson
 - Language Study
 - 1/23/14
 -}
{-
 - Find the penultimate element in list l
 - Returns the second to last element
 -}
 
penultimate l = last (take (length l - 1) l)

penultimate' l = (reverse l) !! 1

penultimate'' l = last (init l)
 
{-
 - Find the element at index k in list l
 - For example: "findK 2 [0,0,1,0,0,0]" returns 1
 -}
 
findK k l = head (drop k l)

findK' k l = [x | x <- l, x == (l !! k)]

{-
 - Determine if list l is a palindrome
 -}

isPalindrome l = if l == (reverse l)
					then "this is a Palindrome"
					else "this is not a Palindrome"

isPalindrome' l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
 
duplicate xs = concat [[x,x] | x <- xs]
 
{-
 - Imitate the functinality of zip
 - The fucntion "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
 
ziplike :: [a] -> [b] -> [(a,b)]
ziplike _ [] = []
ziplike [] _ = []
ziplike (x:xs) (y:ys) = (x,y):ziplike xs ys

ziplike' xs ys = zip xs ys

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])

splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]

dropK k l = [ x | x <- l, x /= (l !! k)]

dropK' k l = (take k l) ++ (drop (k+1) l)

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]

slice i k l = take (k - i) (drop i l)

slice' i k l = [x | x <- l, x < (l !! k), x >= (l !! i)]

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]

insertElem x k l = (take k l) ++ [x] ++ (drop (length (take k l)) l )

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]

-- this solution helps handle if the user gives a value of n that is greater
-- than the length of the list.
rotate n l = if n > length l
				then (drop (n `mod` (length l)) l) ++ (take (n `mod` (length l)) l)
				else (drop n l) ++ (take n l)
 
rotate' n l = (drop n l) ++ (take n l)







 
