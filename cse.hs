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
--ninjaStr::[String]
--ninjaStr=[]
--ninjas::[Ninja]
--ninjas=[]


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
 --getNinjaFromString . words
 let ninjas = parseNinjas listofNinjas
 print (ninjas)
	
-- print (words (head listofNinjas))
 {-getNinja :: IO String
 getNinja = do line <- listofNinjas
					return (read line :: String)-}

 
 --let read (words (head listofNinjas)) :: String
 menu 

{-
content <- readFile (head args) -- get first arg as the file name, read all file to contents
            let linesOfNinjas = lines content -- split contents line by line
            
            let ninjas = parseNinjas linesOfNinjas
------burada okuma yapıyor
-}

parseNinjas :: [String] -> [Ninja] -- Get Ninjas from given String array
parseNinjas [] = []
parseNinjas (x:xs) = (scoreUpdate . getNinjaFromString . words) x : parseNinjas(xs)  


getNinjaFromString :: [String] -> Ninja
getNinjaFromString line = Ninja { -- Create a Ninja element with given String array, convert data types if necessary
name = (line !! 0), -- Name is the first value in csereport
country = (line !! 1), -- country is the second string, but Ninja data type only requires the first character
exam1 = toFloat(line !! 2),  -- exam1 is the third string which is then converted to Float
exam2 = toFloat(line !! 3), -- exam2 is the fourth string which is then converted to Float
ability1 = (line !! 4), -- ability1 is the fifth string 
ability2 = (line !! 5), -- ability2 is the sixth string 
status = "Junior", -- Initial value is junior
r = 0 , -- Initial round number is zero
score = 0 -- Set score initially to zero (This is because we couldn't access the exam1 exam2 ability1 ability2 attributes of this object while creating)
}

  

scoreUpdate :: Ninja -> Ninja
scoreUpdate ninja = updatedNinja where
	updatedNinja =  ninja {score = getScore (exam1 ninja) (exam2 ninja) (ability1 ninja) (ability2 ninja)} where 
	    getScore::Float->Float->String->String->Float
	    getScore e1 e2 a1 a2 = 0.5*e1 + 0.3*e2 + abilityToPoint a1 + abilityToPoint a2

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
 --putStrLn $ unlines ninjaStr
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















