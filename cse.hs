import System.IO 
import System.Environment
import System.Directory  
import Data.List  
import Data.Char
import System.Exit (exitSuccess)

--ninja information
data Ninja=Ninja{name::String,country::Char,score::Float,
status::String,exam1::Float,
exam2::Float,ability1::String,
ability2::String,r::Int}

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

--prints the initial list
--this function also splits data line by line and store it in list 
main::String->IO()
main filename=do
 contents<-readFile filename
 let listofNinjas = lines contents
     ninjaStr = zipWith (\n line -> show line) [0..] listofNinjas
 menu  
 
 --putStrLn "These are my ninjas:"
 --putStrLn $ unlines ninjaStr

--to exit exitSuccess will be used
menu::IO()
menu=do
 putStr  "a)View a Country's Ninja Information\nb)View All Countries' Ninja Information\nc)Make Round Between Ninjas\nd)Make Round Between Countries\ne)Exit\nEnter the action: \n"


 
oneCountryNinjas::IO()
oneCountryNinjas = do
 c<-getChar
 if c=='e' || c=='E' then putStrLn $ unlines earth
 else if c=='l' || c=='L' then putStrLn $ unlines ligthning
 else if c=='w' || c=='W' then putStrLn $ unlines water
 else if c=='n' || c=='N' then putStrLn $ unlines wind
 else if c=='f' || c=='F' then putStrLn $ unlines fire
 










