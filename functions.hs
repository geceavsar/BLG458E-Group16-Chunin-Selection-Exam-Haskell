import System.IO 
import System.Environment
import System.Directory  
import Data.List  
import Data.Char


{-
-------------
This file includes the works we've done to develop the functions for Parts (c) and (d).
After completing the development phase, we copied all of these functions into cse.hs
and from now on, the source code of the project is only in cse.hs.
Please do not try to run this file, probably it won't work.
-------------
-}

-- dummy dataset to test the functions
data Ninja=Ninja{name::String,country::Char,score::Float,
status::String,exam1::Float,
exam2::Float,ability1::Float,
ability2::Float,r::Int} deriving (Eq,Show)

ninjas::[Ninja]

naruto::Ninja
naruto = Ninja{name="Naruto", country='f', status="Junior", exam1=40, exam2=45, ability1=20, ability2=50, score=0, r=0}

sasuke::Ninja
sasuke = Ninja{name="Sasuke", country='f', status="Junior", exam1=50, exam2=60, ability1=50, ability2=40,score=0, r=0}

gaara::Ninja
gaara=Ninja{name="Gaara", country='n', status="Junior", exam1=55, exam2=80, ability1=30, ability2=50,score=0, r=0}

temari::Ninja
temari=Ninja{name="Temari", country='n', status="Junior", exam1=40, exam2=60, ability1=10, ability2=20,score=0, r=0}


ninjas = [naruto, sasuke, gaara, temari]

-- country lists
 
fire::[Ninja]
fire = [ n | n <- ninjas, country n == 'f']


wind::[Ninja]
wind=[ n | n <- ninjas, country n == 'n']


lightning::[Ninja]
lightning=[]
water::[Ninja]
water=[]
earth::[Ninja]
earth=[]

checkIfValid :: String -> Char -> Bool
checkIfValid nm cnt = result where 
    result = length secondList /= 0
    secondList = filter(\n -> country n == cnt) firstList
    firstList = filter(\n -> name n == nm) ninjas

updateScore :: Ninja -> [Ninja]
updateScore winner
    | country winner == 'f' = updateScoreHelper winner fire 
    | country winner == 'e' = updateScoreHelper winner earth 
    | country winner == 'w' = updateScoreHelper winner water
    | country winner == 'l' = updateScoreHelper winner lightning 
    | country winner == 'n' = updateScoreHelper winner wind where
        updateScoreHelper :: Ninja -> [Ninja] -> [Ninja]
        updateScoreHelper winner country = result where
            result = [newNinja] ++ filter(\n -> name n /= name winner) country
            newNinja = 
                if r winner == 2 then winner{score = score winner + 10, r = r winner + 1, status = "Journeyman"} 
                else winner{score = score winner + 10, r = r winner + 1}


deleteLoser :: Ninja -> [Ninja]
deleteLoser loser
    | country loser == 'f' =  filter(\n -> name n /= name loser) fire 
    | country loser == 'e' =  filter(\n -> name n /= name loser) earth 
    | country loser == 'w' =  filter(\n -> name n /= name loser) water
    | country loser == 'l' =  filter(\n -> name n /= name loser) lightning
    | country loser == 'n' =  filter(\n -> name n /= name loser) wind

fight :: Ninja -> Ninja -> (Ninja,Ninja)
fight ninja1 ninja2 
    | score ninja1 > score ninja2 = (ninja1, ninja2)
    | score ninja1 < score ninja2 = (ninja2, ninja1)
    | otherwise = (if ability1 ninja1 + ability2 ninja1 >= ability1 ninja2 + ability2 ninja2 then (ninja1, ninja2) else (ninja2, ninja1))


-- higher order function???????
countryFight :: [Ninja] -> [Ninja] -> (Ninja,Ninja)
countryFight country1 country2 = result where
    result = head country1 `fight` head country2

--higher order
updateLists :: (Ninja, Ninja) -> ([Ninja], [Ninja])
updateLists (winner,loser) = (winnerCountry, loserCountry) where
    winnerCountry = updateScore winner
    loserCountry = deleteLoser loser

descendingRound :: (Ord a) => a -> a -> Ordering
descendingRound firstNinja secondNinja
    | r firstNinja < r secondList = GT
    | r firstNinja > r secondNinja = LT
    | otherwise = EQ

ascendingScore :: (Ord a) => a -> a -> Ordering
ascendingScore firstNinja secondNinja
    | score firstNinja < score secondList = LT
    | score firstNinja > score secondNinja = GT
    | otherwise = EQ

{-groupBy' :: [Ninja] -> [[Ninja]]
groupBy' [] = [[]]
groupBy' (x:xs) = takeWhile(\n -> r n == r x) xs : groupBy' $ dropWhile(\n -> r n == r x) xs-}

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _  [] =  []
groupBy' roundsAreEqual (x:xs) =  (x:ys) : groupBy' roundsAreEqual zs where 
    ys = takeWhile roundsAreEqual xs 
    zs = dropWhile roundsAreEqual xs 

roundsAreEqual :: Ninja -> Ninja -> Bool
roundsAreEqual ninja1 ninja2 = r ninja1 == r ninja2

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger where
    smaller = filter (<x) xs
    larger = filter (>=x) xs

sortNinjas :: [Ninja] -> [Ninja]
sortNinjas country = result where
    --groupedByRound = groupBy (\n1 n2 -> r n1 == r n2) country
    -- groupBy probably groups the items according to their places in the list. (first item is the first group)
    -- sorting and grouping afterwards may be a safer solution
    groupedByRound = groupBy' roundsAreEqual $ reverse $ qsort descendingRound country
    -- sort the scores in ascending order inside each group
    result = concat $ sortEachRound groupedByRound where
        sortEachRound :: [[Ninja]] -> [[Ninja]]
        sortEachRound [] = []
        sortEachRound (x:xs) = (qsort ascendingScore x : sortEachRound xs)


isThereAnyJourneyman :: [Ninja] -> Bool
isThereAnyJourneyman country = result where
    result = (length $ filter(\n -> status n == "Journeyman") country) /= 0

--test commands
--result = fight naruto gaara
--newLists = updateLists result

newLists = updateLists $ fight naruto gaara
winListSorted = sortNinjas $ fst newLists
lsListSorted = sortNinjas $ snd newLists
