import Data.List



-- :r reload un fichier

{-
-- commentaires, multilignes


-}

-- constante entiere, identifiant, declaration typee, definition

c1 :: Int
c1 = 2-1

c2 :: Int
c2 = (-) 2 1


c3 :: Int
c3 = mySub 2 1


mySub :: Int -> Int -> Int
mySub x y = x-y


-- application partielle (et eta reduction)

myNeg :: Int -> Int
myNeg = mySub 0
--myNeg x = mySub 0 x


-- booleen et paresse

b1 :: Bool
b1 = True || (False && not False)

b2 :: Bool
b2 =  1 /= 2 -- (==) :: Int -> Int -> Bool

b3 :: Bool
b3 = 1>2

b4 :: Bool
b4 = undefined

b5 :: Bool
b5 = undefined

-- liste d'entiers, nil, cons, liste en comprehension

l1 :: [Int]
l1 = []

l2 :: [Int]
l2 = 1:l1

l3 :: [Int]
l3 = 2:l2

l4 :: [Int]
l4 = [1,2,4,12,2]

myNil :: [Int]
myNil = []

myCons :: Int -> [Int] -> [Int]
myCons = (:) 

l5 :: [Int]
l5 = [1..10]

l6 :: [Int]
l6 = [1,3..10]

l7 :: [Int]
--l7 = [10,7..12]
l7 = [10,7..0]

-- pattern matching

myHead :: [Int] -> Int
myHead (x:xs) = x

myTail :: [Int] -> [Int]
myTail (x:xs) = xs

-- null

-- n equipes
-- toutes les equipes rencontrent toutes les equipes

-- n-1 equipes, m matchs
-- n equipes, ? matchs m+(n-1)


-- fonction recursive 

myAppend :: [Int] -> [Int] -> [Int] -- aka (++)
myAppend (x:xs) ys = x:myAppend xs ys
myAppend []     ys = ys



myAppend' :: [Int] -> [Int] -> [Int]
myAppend' xs ys | not (null xs) = head xs : myAppend' (tail xs) ys
                | otherwise     = ys


myAppend'' :: [Int] -> [Int] -> [Int]
myAppend'' xs ys | null xs       = ys
                 | not (null xs) = head xs : myAppend'' (tail xs) ys

myAppend4 :: [Int] -> [Int] -> [Int]
myAppend4 (x:xs) ys = 
    let suite = myAppend4 xs ys 
    in x:suite
myAppend4 []     ys = ys

myAppend5 :: [Int] -> [Int] -> [Int]
myAppend5 (x:xs) ys = x:suite where suite = myAppend5 xs ys
myAppend5 []     ys = ys

myAppend6 :: [Int] -> [Int] -> [Int]
myAppend6 xs ys = myAppend6' xs 
    where myAppend6' (x:xs) = x:myAppend6' xs
          myAppend6' []     = ys

-- a vous...

myInit :: [Int] -> [Int]
myInit (x:[]) = []
myInit (x:xs) = x:myInit xs

myLast :: [Int] -> Int
myLast (x:[]) = x
myLast (x1:xs) = myLast xs

myNull :: [Int] -> Bool
myNull [] = True
myNull (x:xs) = False

myNull' :: [Int] -> Bool
myNull' = undefined

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1+myLength xs

myReverse :: [Int] -> [Int]
myReverse [x] = [x]
myReverse xs = myLast xs:myReverse (myInit xs)


-- iteratif, comparer les complexites experimentalement
myReverse' :: [Int] -> [Int]
myReverse' = undefined
--myReverse' [x] = [x]
--myReverse' xs = xs[myLength xs]:myReverse (myInit xs)

myConcat :: [Int] -> [Int] -> [Int]
myConcat ([x]) ys  = x:ys
myConcat xs ys = (myConcat (init xs) ((myLast xs) :ys))

myAnd :: [Bool] -> Bool
myAnd ([x])  = x
myAnd (x:xs)  = x && (myAnd xs)

myOr ::  [Bool] -> Bool
myOr ([x])  = x
myOr (x:xs)  = x || (myOr xs)

myProduct :: [Int] -> Int
myProduct ([x])  = x
myProduct (x:xs)  = x * (myProduct xs)

-- pas d'element neutre pour max et min 

myTake :: Int -> [Int] -> [Int]
myTake = undefined

myDrop :: Int -> [Int] -> [Int]
myDrop = undefined

-- cette fonction existe sous le nom !!
myBangBang :: [Int] -> Int -> Int
myBangBang = undefined

-- liste deja triee
myInsert :: Int -> [Int] -> [Int]
myInsert = undefined

mySort :: [Int] -> [Int]
mySort = undefined