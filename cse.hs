import System.IO 
import System.Environment
import System.Directory  
import Data.List  
import Data.Char
import System.Exit (exitSuccess)
import Control.Monad
--ninja information
data Ninja=Ninja{name::String,country::String,score::Float,
status::String,exam1::Float,
exam2::Float,ability1::String,
ability2::String,r::Int} deriving (Eq,Show)

--country list
fire::[Ninja]
ligthning::[Ninja]
water::[Ninja]
wind::[Ninja]
earth::[Ninja]


--functions as algebraic data types
data Function = OneCountryNinjas | AllCountriesNinjas | MakeRoundNinjas |MakeRoundCountries |Exit deriving Show

--our main function
main=do
 contents<-readFile "csereport.txt"--reads from file
 let listofNinjas = lines contents--ignores newline characters and stores lines in a list
 let ninjas = convertToNinjas listofNinjas--each line that stored is converted to ninja
 assignCountries ninjas
 [earth,wind,lightning,[]]
 menu 
 do 
  forever $ do
  choise<-getLine--user enters a function choise
  let c=pickFunction choise--related function is determined
  case c of--and called in here below
    OneCountryNinjas->oneCountryNinjas ninjas
    AllCountriesNinjas->allCountryNinjas ninjas
    --MakeRoundNinjas->makeRoundNinjas ninjas
   --MakeRoundCountries->makeRoundCountries
    Exit->exit

menu::IO()
menu=do
 putStr  "a)View a Country's Ninja Information\nb)View All Countries' Ninja Information\nc)Make Round Between Ninjas\nd)Make Round Between Countries\ne)Exit\nEnter the action: \n"
 {-do 
  choise<-getLine
  let c=pickFunction choise
  case c of
   OneCountryNinjas->oneCountryNinjas
  --AllCountriesNinjas->allCounrtiesNinjas
  --MakeRoundNinjas->makeRoundNinjas
  --MakeRoundCountries->makeRoundCountries
   Exit->exit
-}

--when user enters one of the strings below(a,b,c,d,e) related function will be called
pickFunction::String->Function
pickFunction f=case f of 
    "a"->OneCountryNinjas
    "b"->AllCountriesNinjas
    "c"->MakeRoundNinjas
    "d"->MakeRoundCountries
    "e"->Exit

assignCountries::[Ninja]->[[Ninja]]
assignCountries [] = []
assignCountries (x:xs) = insert x : assignCountries (xs)
 where 
  insert::Ninja->[Ninja]
  insert ninja
   |country ninja=="Earth" = ninja:earth 
   |country ninja=="Fire" = ninja:fire 
   |country ninja=="Lightning" = ninja:lightning 
   |country ninja=="Water" = ninja:water 
   |country ninja=="Wind" = ninja:wind
-- let earth=[n|n<-ninjas,country n=="Earth"]
 
--make round between ninjas
{-makeRoundNinjas::[Ninja]
makeRoundNinjas ninjas=do
 putStrLn "Enter the name of the first ninja: "
 name1<-getLine
 putStrLn "Enter the country of the first ninja: "
 country1<-getLine
 putStrLn "Enter the name of the second ninja: "
 do
  name2<-getLine
  fight [n|n <- ninjas, name n==name1] [n|n <- ninjas, name n==name2]
-}

--chosen 2 ninjas are faught 
fight :: Ninja -> Ninja -> (Ninja,Ninja)
fight ninja1 ninja2 
    | score ninja1 > score ninja2 = (ninja1, ninja2)
    | score ninja1 < score ninja2 = (ninja2, ninja1)
    | otherwise = (if ability1 ninja1 + ability2 ninja1 >= ability1 ninja2 + ability2 ninja2 then (ninja1, ninja2) else (ninja2, ninja1))

--this function updates the score of a ninja after fight
updateScore :: Ninja -> [Ninja]
updateScore winner
    | country winner == "Fire" = updateScoreHelper winner fire 
    | country winner == "Earth" = updateScoreHelper winner earth 
    | country winner == "Water" = updateScoreHelper winner water
    | country winner == "Lightning" = updateScoreHelper winner lightning 
    | country winner == "Wind" = updateScoreHelper winner wind where
        updateScoreHelper :: Ninja -> [Ninja] -> [Ninja]
        updateScoreHelper winner country = result where
            result = [newNinja] ++ filter(\n -> name n /= name winner) country
            newNinja = 
                if r winner == 2 then winner{score = score winner + 10, r = r winner + 1, status = "Journeyman"} 
                else winner{score = score winner + 10, r = r winner + 1}

--higher order
updateLists :: (Ninja, Ninja) -> ([Ninja], [Ninja])
updateLists (winner,loser) = (winnerCountry, loserCountry) where
    winnerCountry = updateScore winner
    loserCountry = deleteLoser loser
--updates the score of winner ninja
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

--deletes  the ninja who lost the current round
deleteLoser :: Ninja -> [Ninja]
deleteLoser loser
    | country loser == 'f' =  filter(\n -> name n /= name loser) fire 
    | country loser == 'e' =  filter(\n -> name n /= name loser) earth 
    | country loser == 'w' =  filter(\n -> name n /= name loser) water
    | country loser == 'l' =  filter(\n -> name n /= name loser) lightning
    | country loser == 'n' =  filter(\n -> name n /= name loser) wind


--string to ninja conversion
convertToNinjas :: [String] -> [Ninja]
convertToNinjas [] = []--if empty return 
convertToNinjas (x:xs) = (scoreUpdate . getNinja . words) x : convertToNinjas(xs)--each line is divided into sublist without spaces and obtained string list is sent to getNinja and returns as ninja type 

--each string object will be assigned as ninja object with additional score,status and round information
getNinja :: [String] -> Ninja
getNinja line = Ninja {name = (line !! 0),country = (line !! 1),exam1 = toFloat(line !! 2),exam2 = toFloat(line !! 3),ability1 = (line !! 4),ability2 = (line !! 5),status = "Junior",r = 0,score=0}

--this function sets the score of a ninja which is initially set to zero 
scoreUpdate :: Ninja -> Ninja
scoreUpdate ninja = updatedNinja where
 updatedNinja =  ninja {score = getScore (exam1 ninja) (exam2 ninja) (ability1 ninja) (ability2 ninja)} where 
   getScore::Float->Float->String->String->Float
   getScore e1 e2 a1 a2 = 0.5*e1 + 0.3*e2 + abilityToPoint a1 + abilityToPoint a2

--convert type string to type float
toFloat::String->Float
toFloat s = read s::Float



--in this function user enters a country code and required countrys' ninjas printed
oneCountryNinjas::[Ninja]->IO()
oneCountryNinjas ninjas=do
 putStrLn "Enter the country code: "
 do
  code<-getLine
  case code of
   "e"->print [ n | n <- ninjas, country n == "Earth"]
   "E"->print [ n | n <- ninjas, country n == "Earth"]
   "l"->print [ n | n <- ninjas, country n == "Lightning"]
   "L"->print [ n | n <- ninjas, country n == "Lightning"]
   "w"->print [ n | n <- ninjas, country n == "Water"]
   "W"->print [ n | n <- ninjas, country n == "Water"]
   "n"->print [ n | n <- ninjas, country n == "Wind"]
   "N"->print [ n | n <- ninjas, country n == "Wind"]
   "f"->print [ n | n <- ninjas, country n == "Fire"]
   "F"->print [ n | n <- ninjas, country n == "Fire"]

--list is already sorted, so in this function we only print blindly
allCountryNinjas::[Ninja]->IO()
allCountryNinjas ninjas=print ninjas

--exit function
exit::IO()
exit=do
 putStrLn "Final List:"
 --here we must print latest version of ninja list
 return()
   
--we take abilities as string and return their float equivalents which are already given in the assignment sheet
abilityToPoint::String->Float
abilityToPoint str
 |str=="Clone" = 20
 |str=="Hit" = 10
 |str=="Lightning" = 50
 |str=="Vision" = 30
 |str=="Sand" = 50
 |str=="Fire" = 40
 |str=="Water" = 30
 |str=="Blade" = 20
 |str=="Summon" = 50
 |str=="Storm" = 10
 |str=="Rock" = 20
 |otherwise=0














