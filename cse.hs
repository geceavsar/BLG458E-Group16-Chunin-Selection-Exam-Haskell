import System.IO 
import System.Environment
import System.Directory  
import Data.List  
import Data.Char
import System.Exit (exitSuccess)

--ninja information
data Ninja=Ninja{name::String,country::String,score::Float,
status::String,exam1::Float,
exam2::Float,ability1::String,
ability2::String,r::Int} deriving (Eq,Show)

--country list
fire::[Ninja]
fire=[]
ligthning::[Ninja]
ligthning=[]
water::[Ninja]
water=[]
wind::[Ninja]
wind=[]
earth::[Ninja]
earth=[]
ninjaStr::[String]
ninjaStr=[]
ninjas::[Ninja]
ninjas=[]

--functions//algebraic data types
data Function = OneCountryNinjas | AllCountriesNinjas | MakeRoundNinjas |MakeRoundCountries |Exit deriving Show

--choose a function
pickFunction::String->Function
pickFunction f=case f of 
    "a"->OneCountryNinjas
    "b"->AllCountriesNinjas
    "c"->MakeRoundNinjas
    "d"->MakeRoundCountries
    "e"->Exit

--prints the initial list
--this function also splits data line by line and store it in list 
main=do
 contents<-readFile "csereport.txt"
 let listofNinjas = lines contents
     ninjaStr = zipWith (\n line -> show line) [0..] listofNinjas     
 putStrLn $ unlines ninjaStr
 menu 
 --putStrLn "These are my ninjas:"
 --putStrLn $ unlines ninjaStr
 
convert2Ninja::[String]->[Ninja]
convert2Ninja ninjaStr
 |null  ninjaStr= []
 |otherwise = myparser (head ninjaStr)
 where
  myparser::[String]->[Ninja]
  myparser s = Ninja s !! 0 s !! 1 getScore (toFloat s !! 2) (toFloat s !! 3) s !! 4 s !! 5 "Junior" (read (s !! 2)::Float) (toFloat s !! 3) s !! 4 s !! 5 0.0) ++ ninjas

toFloat::String->Float
toFloat s = read s::Float

--print main menu
menu::IO()
menu=do
 putStr  "a)View a Country's Ninja Information\nb)View All Countries' Ninja Information\nc)Make Round Between Ninjas\nd)Make Round Between Countries\ne)Exit\nEnter the action: \n"
 do 
  choise<-getLine
  let c=pickFunction choise
  case c of
   OneCountryNinjas->oneCountryNinjas
  --AllCountriesNinjas->allCounrtiesNinjas
  --MakeRoundNinjas->makeRoundNinjas
  --MakeRoundCountries->makeRoundCountries
   Exit->exit

--in this function user enters a counrty code and required countrys' ninjas printed
oneCountryNinjas::IO()
oneCountryNinjas=do
 putStrLn "Enter the country code: "
 do
  code<-getLine
  case code of
   "e"->print earth
   "E"->print earth
   "l"->print ligthning
   "L"->print ligthning
   "w"->print water
   "W"->print water
   "n"->print wind
   "N"->print wind
   "f"->print fire
   "F"->print fire
--exit function
exit::IO()
exit=do
 putStrLn "Final List:"
 putStrLn $ unlines ninjaStr
 return()
   
--abilities as score equivalents
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

--this is a higher order function cause it returns a function as result
getScore::Float->Float->String->String->Float
getScore e1 e2 a1 a2 = 0.5*e1 + 0.3*e2 + abilityToPoint a1 + abilityToPoint a2


