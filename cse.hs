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
{-fire::[Ninja]
fire=[]
ligthning::[Ninja]
ligthning=[]
water::[Ninja]
water=[]
wind::[Ninja]
wind=[]
earth::[Ninja]
earth=[]
-}

--functions as algebraic data types
data Function = OneCountryNinjas | AllCountriesNinjas | MakeRoundNinjas |MakeRoundCountries |Exit deriving Show

--when user enters one of the strings below(a,b,c,d,e) related function will be called
pickFunction::String->Function
pickFunction f=case f of 
    "a"->OneCountryNinjas
    "b"->AllCountriesNinjas
    "c"->MakeRoundNinjas
    "d"->MakeRoundCountries
    "e"->Exit

--our main function
main=do
 contents<-readFile "csereport.txt"--reads from file
 let listofNinjas = lines contents--ignores newline characters and stores lines in a list
 let ninjas = convertToNinjas listofNinjas--each line that stored is converted to ninja
 menu 
 do
  choise<-getLine
  let c=pickFunction choise
  case c of
    OneCountryNinjas->oneCountryNinjas ninjas
    AllCountriesNinjas->allCountryNinjas ninjas
    MakeRoundNinjas->makeRoundNinjas ninjas
   --MakeRoundCountries->makeRoundCountries
    Exit->exit

makeRoundNinjas::[Ninja]->(Ninja,Ninja)
makeRoundNinjas=do
 putStrLn "Enter the name of the first ninja: "
 name<-getLine
 putStrLn "Enter the country code of the first ninja: "
 countryCode<-getLine
 putStrLn "Enter the name of the second ninja: "
 name2<-getLine
 putStrLn "Enter the country code of the second ninja: "
 countryCode2<-getLine
 fight [n|n <- ninjas, name n==name] [n|n <- ninjas, name n==name2]

fight :: Ninja -> Ninja -> (Ninja,Ninja)
fight ninja1 ninja2 
    | score ninja1 > score ninja2 = (ninja1, ninja2)
    | score ninja1 < score ninja2 = (ninja2, ninja1)
    | otherwise = (if ability1 ninja1 + ability2 ninja1 >= ability1 ninja2 + ability2 ninja2 then (ninja1, ninja2) else (ninja2, ninja1))

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
convertToNinjas :: [String] -> [Ninja]--string to ninja
convertToNinjas [] = []--if empty return 
convertToNinjas (x:xs) = (scoreUpdate . getNinja . words) x : convertToNinjas(xs)--each line is divided into sublist without spaces and obtained string list is sent to getNinja and returns as ninja type 


getNinja :: [String] -> Ninja
getNinja line = Ninja {name = (line !! 0),country = (line !! 1),exam1 = toFloat(line !! 2),exam2 = toFloat(line !! 3),ability1 = (line !! 4),ability2 = (line !! 5),status = "Junior",r = 0,score=0}

  

scoreUpdate :: Ninja -> Ninja
scoreUpdate ninja = updatedNinja where
 updatedNinja =  ninja {score = getScore (exam1 ninja) (exam2 ninja) (ability1 ninja) (ability2 ninja)} where 
   getScore::Float->Float->String->String->Float
   getScore e1 e2 a1 a2 = 0.5*e1 + 0.3*e2 + abilityToPoint a1 + abilityToPoint a2

toFloat::String->Float
toFloat s = read s::Float



--in this function user enters a counrty code and required countrys' ninjas printed
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

allCountryNinjas::[Ninja]->IO()
allCountryNinjas ninjas=print ninjas

--exit function
exit::IO()
exit=do
 putStrLn "Final List:"
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














