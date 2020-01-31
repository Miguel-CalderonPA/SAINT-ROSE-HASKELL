--Miguel Calderon
--Date 9/19/18
--Last Modifed [9/26/19] 
--Professor Cornell Lab #1
---------------------------------------------------------------
{-
 - Question #1
 - 1----Defines function
 - 2----Uses mod function to determine even or odd
 - and returns bool, true if odd, otherwise false
 -}
myOdd :: Integer -> Bool-- 1
myOdd num
        | ans == 0 = False
        | otherwise = True
         where ans = (num `mod` 2)--2

{-
 - List Version
 - 1----Defines function
 - 2----Determines if odd or even and displays Bool based on it
-}
myOddL :: Integer -> Bool--1
myOddL num =head[if num `mod` 2 == 1 then True else False]--2

---------------------------------------------------------------
{-
 - Recursive Version
 - Question #2
 - 1----Defines function
 - 2----base case for empty list
 - 3----Determines if current element is equal to target
 - otherwise keep searching
 -}
contains :: (Eq a) => a -> [a] -> Bool-- 1
contains _ [] = False--2
contains value (e:list)
           | e == value = True
           | otherwise = contains value list-- 3

{-
 - List Version
 - 1----Defines function
 - 2----base case for an empty list
 - 3----searches in list for element if not found then false otherwise True
 -}
containsL :: (Eq a) => a -> [a] -> Bool--1
containsL _ [] = False--2
containsL value (e:list) =head[if e==value then True else containsL value list] --3

---------------------------------------------------------------
{-
 - Question #3
 - 1----Defines function
 - 2----If not in list there is no point to search it
 -}
numOccurences :: (Eq a) => a -> [a] -> Int --1
numOccurences value list = if value `elem` list
                           then rFind value list ((length list)-1)
                           else 0 --2

{-
 - Find Function
 - 1----Define function
 - 2----base case if list is empty then numOccurences
 - will send -1 because length() -1
 - 3----sees if current element is target otherwise keep searching,
 - or it found add one for callback to add
 -}
rFind :: (Eq a) => a -> [a] -> Int -> Int--1
rFind value list (-1) = 0--2
rFind value list i
              | (list !! i) == value = 1 + rFind value list (i-1)
              | otherwise = rFind value list (i-1)--3
{-
 - List Version
 - 1----Defines function
 - 2----Base case for an empty list
 - 3----gets a list of repeated occurences then takes length of that
 -}
numOccurencesL :: (Eq a) => a -> [a] -> Int--1
numOccurencesL _ [] = 0--2
numOccurencesL value list =length'[e | e<-list, e==value]--3

{-
 - Length' Helper function
 - 1----Define function
 - 2----base case for an empty list
 - 3----counts the elements of the list
 -}
length' :: [a]->Int--1
length' [] = 0--2
length' (e:list) = 1 + length list--3

---------------------------------------------------------------
{-
 - Question #4
 - Recursive Version
 - 1----Defines function
 - 2----base case for if left empty and right not empty
 - 3----base case for if left not empty and right empty
 - 4----base case for if both left and right are empty
 - 5----if here then it decides if that index is equal in both lists
 - if so continue
 -}
equal ::(Eq a) => [a] -> [a] -> Bool--1
equal [] (elem:list) = False--2
equal (elem:list) [] = False--3
equal [] [] = True--4
equal (elem:list1) (e:list2) 
               |e == elem = equal list1 list2
               | otherwise = False--5

{-
 - List Version
 - 1----Defines function
 - 2----base case for empty left list
 - 3----base case for empty right list
 - 4----base case for 2 empty list
 - 5----keeps testing elements of list until end or false then reports Bool
 -}
equalL :: (Eq a) => [a] -> [a] -> Bool
equalL [] (e:list) = False
equalL (e:list) [] = False
equalL [] [] = True
equalL (e:list) (e2:list2) = head[if e==e2 then equalL list list2 else False]

---------------------------------------------------------------
{-
 - Question #5
 - Recursive Version
 - 1----Defines function
 - 2----base case for if list is empty
 - 3----base case for if one element
 - 4----if here then this will take element off add 1 then add it back
 -}
addOne :: [Integer] -> [Integer]--1
addOne [] = [0]--2
addOne [elem] = [elem+1]--3
addOne (elem:list) = [elem + 1] ++ addOne list--4

{-
 - List Version
 - 1----Defines function
 - 2----base case for empty list
 - 3----base case for list of 1 element
 - 4----adds one to all elements
-}
addOneL ::  [Integer] -> [Integer]--1
addOneL [] = [0]--2
addOneL [e] = [e+1]--3
addOneL list = [e+1 | e<-list]--4

---------------------------------------------------------------
{-
 - Question #6
 - List Version
 - 1----Defines function
 - 2----takes each element and only returns it if its odd
 -}
justOdds::[Integer] -> [Integer]--1
justOdds list = [ e | e <-list, e `mod` 2 == 1]--2

{-
 - Recursive Version 
 - 1----Defines function
 - 2----if e is odd it puts it in a list
 -}
justOddsL :: [Integer] -> [Integer]--1
justOddsL list = [e | e<-list, e `mod` 2 == 1]--2

---------------------------------------------------------------
{-
 - Question #7
 - List Version
 - 1----Defines function
 - 2----I'm basically redefining filter here
 -}
removeAll :: (Eq a) => a -> [a] -> [a]--1
removeAll value list = filter (value/=) list--2

{-
 - Recursive function
 - 1----Defines function
 - 2----base case for an empty list
 - 3----if value equals element then don't include
 -}
removeAllL :: (Eq a) => a -> [a] -> [a]--1
removeAllL _ [] = []--2
removeAllL value (e:list)
             | e==value = removeAll value list
             | otherwise = e : removeAll value list--3

---------------------------------------------------------------
{-
 - Question #8
 - Recursive Version (1)
 - 1----Defines function
 - 2----base case for if list empty
 - 3----if here check element to see if target otherwise keep going
 -}
removeFirst :: (Eq a) => a -> [a] -> [a]--1
removeFirst _ [] = []--2
removeFirst value (e:list) 
                       | e == value = list
                       | otherwise =e : removeFirst value list--3
{-
 - Attempted List Version
 - 1----Defines function
 - 2----base case for an empty list
 - 3----tried to place true on found elements then remove,
 - but couldn't figure it out
 -}
--bools = [False,False,False,False,False,False]
--removeFirst :: (Eq a) => a -> [a] -> [a] 
--removeFirst _ [] = []
--removeFirst value list = [if x == value then y == True | (x,y) <- list `zip` bools]

----------------------------------------------------------------
{-
 - Question #9
 - Recursive Version (1)
 - 1----Defines function
 - 2----calls recursive function with True added
 - telling next function where to start
 -}
everyOther::(Eq a) => [a] -> [a]--1
everyOther list = rEveryOther list True--2

{-
 - rEveryOther function
 - 1----Defines function
 - 2----base case for if empty list
 - 3----base case for one element and False
 - 4----base case for one element and True
 - 5----if here if will flip bools showing every other
 -}
rEveryOther :: (Eq a) => [a] -> Bool -> [a]--1
rEveryOther [] _ = []--2
rEveryOther [e] False = []--3
rEveryOther [e] True = [e]--4
rEveryOther (e:list) bool 
                      | bool == True = e : rEveryOther list False
                      | otherwise = rEveryOther list True--5

{-
 - List Version
 - 1----Define function
 - 2----base case for an empty list
 - 3----if index is odd don't include it
 -}
everyOtherL :: (Eq a) => [a] -> [a]--1
everyOtherL [] = [] --2
everyOtherL list = [e | e<-list, (indexOf e list) `mod` 2 ==0]--3

-----------------------------------------------------------------
{-
 - Question #10
 - Recursive Version
 - 1----Defines function
 - 2----base case for if empty list
 - 3----checks for poor input otherwise jump to helper
 -}
get :: Int -> [a] -> a--1
get index [] = error "Index out of bounds"--2
get index list
              | index < 0 = error "Index out of bounds"
              | index >= (length list) = error "Index out of bounds"
              | otherwise = rGet index list 0--3

{-
 - rGet function
 - 1----Defines function
 - 2----the added var here is traverse
 - allowing to let index keep the orignal value to compare with
 -}
rGet :: Int -> [a] -> Int -> a--1
rGet index (e:list) traverse
                | index == traverse = e
                | otherwise = rGet index list (traverse+1)--2
{-
 - List Version
 - 1----Defines function
 - 2----base case for invalid index
 - 3----zips list with an index list, and once match returns x coord or tuple
 - my head function has preset exception for out of bounds
 -}
getL :: Int -> [a] -> a--1
getL index [] = error "Index out of bounds"--2
getL index list =grabElem[ x | (x,y) <- list `zip` [0..(length' list)], y==index]--3

{-
 -grabElem helper
 - 1----Defines function
 - 2----base case for empty list, in this case its being used specifically for getL
 - 3----returns first element
 -}
grabElem :: [a] -> a--1
grabElem [] = error ("Index out of bounds")--2
grabElem (e:list) = e--3

------------------------------------------------------------------
{-
 - Question 11
 - Recursive Version
 - 1----Defines function
 - 2----base case for if empty list to start with
 - 3----if here checks if this element is the target,
 - then if its even in the rest of the list, else keep going
 -}
indexOf :: (Eq a) => a -> [a] -> Int--1
indexOf _ [] = (-1)--2
indexOf value (e:list)
                      | value == e = 0
                      | contains value list == False = -1
                      | otherwise = 1 + indexOf value list--3
{-
 - List Version
 - 1----Defines function
 - 2----base case for if list is empty
 - 3----zips list with index list, and if found returns index, 
 - head has exception handling for not found
 -}
indexOfL :: (Eq a) => a-> [a] -> Int--1
indexOfL _ [] = (-1)--2
indexOfL value list =head[ y | (x,y) <- list `zip` [0..(length' list)], x==value]--3

------------------------------------------------------------------
{-
 - Question 12
 - Recurive
 - 1----Defines function
 - 2----base case for if empty
 - 3----if last value is not it keep traversing backwards and on the
 - way back up chain the elements back together
 -}
removeLast :: (Eq a) => a -> [a] -> [a]--1
removeLast _ [] = []--2
removeLast value list
                   | last list == value = init (list) 
                   | otherwise = (removeLast value (init list)) ++ [last (list)]--3

{-
 - Recursive Version (2)
 - 1----Defines function
 - 2----base case for if list is empty
 - 3---- takes the reverse list, removes first occurence, then
 - reverses it back so now the last occurence is gone
 -}
removeLast' :: (Eq a) => a-> [a] -> [a]--1
removeLast' _ [] = []--2
removeLast' value list = reverse'(removeFirst value (reverse' list))--3

{-
 - Reverse helper function 
 - 1----Defines function
 - 2----base case for empty list
 - 3----breaks down list and adds on backwards
 -}
reverse' :: [a] -> [a]--1
reverse' [] = []--2
reverse' (e:list) = reverse' list ++ [e]--3


-----------------------------------------------------------------------
