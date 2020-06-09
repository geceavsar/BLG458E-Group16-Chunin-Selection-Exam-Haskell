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

--functions as algebraic data types
data Function = OneCountryNinjas | AllCountriesNinjas | MakeRoundNinjas |MakeRoundCountries |Exit deriving Show

--our main function
main=do
 contents<-readFile "csereport.txt"--reads from file
 let listofNinjas = lines contents--ignores newline characters and stores lines in a list
 let ninjas = convertToNinjas listofNinjas--each line that stored is converted to ninja
 let listOfCountries=separateCountries ninjas
 menu listOfCountries


separateCountries :: [Ninja] -> [[Ninja]]
separateCountries ninjaList = countryList where
    countryList = [earth, lightning, water, wind, fire] where
        earth = [ n | n <- ninjaList, country n == "Earth"]
        lightning = [ n| n <- ninjaList, country n == "Lightning"]
        water = [ n | n <- ninjaList, country n=="Water"]
        wind = [ n |  n <- ninjaList, country n=="Wind"]
        fire = [ n | n <- ninjaList, country n=="Fire"]
 
menu::[[Ninja]]->IO()
menu countryList=do
 putStr  "a)View a Country's Ninja Information\nb)View All Countries' Ninja Information\nc)Make Round Between Ninjas\nd)Make Round Between Countries\ne)Exit\nEnter the action: \n"
 do 
  choise<-getLine
  let c=pickFunction choise
  case c of
   OneCountryNinjas->do Main.oneCountryNinjas countryList
                        menu countryList
   AllCountriesNinjas->do print countryList
                          menu countryList
   MakeRoundNinjas->do  putStrLn "Enter the name of the first ninja: "
                        name1<-getLine
                        putStrLn "Enter the country code of the first ninja."
                        countryCode1 <- getLine
                        putStrLn "Enter the name of the second ninja: "
                        name2<-getLine
                        putStrLn "Enter the country code of the second ninja."
                        countryCode2 <- getLine
                        let country1 = countryCodeToCountry countryCode1
                        let country2 = countryCodeToCountry countryCode2
                        if Main.checkIfValid countryList name1 country1 && Main.checkIfValid countryList name2 country2
                            then 
                                do
                                    let ninja1 = (filter(\n -> name n == name1) (concat countryList)) !! 0
                                    let ninja2 = (filter(\n -> name n == name2) (concat countryList)) !! 0
                                    menu $ Main.updateLists countryList $ Main.fight ninja1 ninja2
                            else 
                                do
                                    putStrLn "Names or countries of the ninjas do not match. Try again."
                                    menu countryList
   MakeRoundCountries->do
                        putStrLn "Enter the country code of the first ninja."
                        countryCode1 <- getLine
                        putStrLn "Enter the country code of the second ninja."
                        countryCode2 <- getLine
                        let country1 = countryCodeToIndex countryCode1
                        let country2 = countryCodeToIndex countryCode2
                        if country1 /= -1 && country2 /= -1
                            then
                                do
                                    menu $ Main.updateLists countryList $ Main.countryFight (countryList !! country1) (countryList !! country2)
                            else
                                do
                                    putStrLn "Countries of the ninjas do not match. Try again."
                                    menu countryList

       
   Exit-> do print countryList
             return ()


pickFunction::String->Function
pickFunction f=case f of 
    "a"->OneCountryNinjas
    "b"->AllCountriesNinjas
    "c"->MakeRoundNinjas
    "d"->MakeRoundCountries
    "e"->Exit

checkIfValid :: [[Ninja]] -> String -> String -> Bool
checkIfValid ninjaList nm cnt = result where 
    result = not $ null secondList
    secondList = filter(\n -> country n == cnt) firstList
    firstList = filter(\n -> name n == nm) (concat ninjaList)

--chosen 2 ninjas are faught 
fight :: Ninja -> Ninja -> (Ninja,Ninja)
fight ninja1 ninja2 
    | score ninja1 > score ninja2 = (ninja1, ninja2)
    | score ninja1 < score ninja2 = (ninja2, ninja1)
    | otherwise = (if abilityToPoint (ability1 ninja1) + abilityToPoint (ability2 ninja1) >= abilityToPoint (ability1 ninja2) + abilityToPoint (ability2 ninja2) then (ninja1, ninja2) else (ninja2, ninja1))


--countryfight
countryFight :: [Ninja] -> [Ninja] -> (Ninja,Ninja)
countryFight country1 country2 = result where
    result = head country1 `fight` head country2

--this function updates the score of a ninja after fight
updateWinner :: [Ninja] -> Ninja -> [Ninja]
updateWinner countryList winner = updateWinnerHelper winner countryList where
        updateWinnerHelper :: Ninja -> [Ninja] -> [Ninja]
        updateWinnerHelper winner country = result where
            result = [newNinja] ++ filter(\n -> name n /= name winner) country
            newNinja = 
                if r winner == 2 then winner{score = score winner + 10, r = r winner + 1, status = "Journeyman"} 
                else winner{score = score winner + 10, r = r winner + 1}


updateLists :: [[Ninja]] -> (Ninja, Ninja) -> [[Ninja]]
updateLists countryList (winner,loser) = separateCountries winnerCountry where
    winnerCountry = updateWinner (deleteLoser countryList loser) winner

--deletes  the ninja who lost the current round
deleteLoser :: [[Ninja]] -> Ninja -> [Ninja]
deleteLoser countryList loser = filter(\n -> name n /= name loser) (concat countryList)

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
oneCountryNinjas::[[Ninja]]->IO()
oneCountryNinjas ninjas=do
 putStrLn "Enter the country code: "
 do
  code<-getLine
  case code of
   "e"->print $ ninjas !! 0
   "E"->print $ ninjas !! 0
   "l"->print $ ninjas !! 1
   "L"->print $ ninjas !! 1
   "w"->print $ ninjas !! 2
   "W"->print $ ninjas !! 2
   "n"->print $ ninjas !! 3
   "N"->print $ ninjas !! 3
   "f"->print $ ninjas !! 4
   "F"->print $ ninjas !! 4

countryCodeToCountry :: String -> String
countryCodeToCountry code 
    | code == "e" || code == "E" = "Earth"
    | code == "l" || code == "L" = "Lightning"
    | code == "w" || code == "W" = "Water"
    | code == "n" || code == "N" = "Wind"
    | code == "f" || code == "F" = "Fire"
    | otherwise = "Null"

countryCodeToIndex :: String -> Int
countryCodeToIndex code 
    | code == "e" || code == "E" = 0
    | code == "l" || code == "L" = 1
    | code == "w" || code == "W" = 2
    | code == "n" || code == "N" = 3
    | code == "f" || code == "F" = 4
    | otherwise = -1


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














