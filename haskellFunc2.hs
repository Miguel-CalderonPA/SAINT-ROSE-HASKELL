{- QUESTION 1
 - Extract a slice from a list. Given two indices, i and k, 
 - the slice is the list containing the elements between the 
 - i'th and k'th element of the original list (both limits included). 
 - Start counting the elements with 1. This must work for any type of list. 
 - Prelude> slice['a','b','c','d','e','f','g','h','i','k'] 3 7
 - "cdefg"
 -}

{- SLICE Q1
 - --1: Function Definition
 - --2: case for empty list
 - --3: catch all branch
 - --4: checks range
 - --5: checks to make sure input is positive
 - --6: if all is well send it to kimbo slice
 - --7: function definintion
 - --8: catch - error checking already done
 - --9: creates a way to slice a list with backwards indexes
 - --10:catch all actually grabs the slice
 - --11:increase readabilty
 - --12:increase readabilty
-}

--Couldn't resist, this essentially just renames slice
slice :: [a] -> Int -> Int -> [a] --1
slice [] _ _ = [] --2
slice list begin end --3
        | end > (length' list) || begin > (length' list) = error "OUT OF RANGE" --4
        | end < 1 || begin < 1 = error "INDEX MUST BE GREATER THAN 0" --5
        | otherwise = kimbo list begin end--6

kimbo :: [a] -> Int -> Int -> [a]--7
kimbo list begin end--8
        | begin > end = kimbo (reverse' list) (((length' list) - begin) + 1) (((length' list) - end) + 1) --9
        | otherwise = take cake fiveSecondRule--10
          where cake           = (end - begin + 1)--11
                fiveSecondRule = (drop (begin - 1) list)--12

-- Determines the length of a list with bases cases otherwise counts per element
length' :: [a] -> Int
length' [] = 0
length' [x] = 1
length' (e:list) = 1 + length' list

-- Reverses the list with base cases otherwise it recursively creates the reverse list
reverse'::[a] -> [a]
reverse' []  = []
reverse' [x] = [x]
reverse' (e:list) =  reverse' list ++ [e]

{- Rotate a list N places to the left. This must work for a list of any type.
 - Hint: use a predefined functions length and (++).
 - Prelude>rotate['a','b','c','d','e','f','g','h'] 3
 - "defghabc"
 - Prelude>rotate['a','b','c','d','e','f','g','h'] (-2)
 - "ghabcdef"
 -}

{- --1: function definition
 - --2: case for empty list
 - --3: case for a single element list
 - --4: catch all
 - --5: case for if shift is larger than list
 - --6: case for if shift is less than -list
 - --7: case for negatives
 - --8: case for positives
 - --9: increase readability
 -}

rotate::(Eq a) => [a] -> Int -> [a]--1
rotate [] _  = []--2
rotate [x] _ = [x]--3
rotate list shift--4
              | shift > theLength = rotate list (shift - theLength)--5
              | shift < ((-1) * theLength) = rotate list (shift + theLength)--6
              | shift < 0 = take theLength (drop (theLength + shift) list) ++ take (theLength + shift) list--7
              | otherwise =  (take theLength (drop shift list)) ++ take shift list--8
              where theLength = (length' list)--9
