import System.IO 
import System.Environment
import System.Directory  
import Data.List  
import Data.Char

data Ninja=Ninja{name::String,country::Char,score::Float,
status::String,exam1::Float,
exam2::Float,ability1::String,
ability2::String,r::Int} deriving (Eq,Show)

ninjas::[Ninja]

naruto::Ninja
naruto = Ninja{name="Naruto", country='f', status="Junior", exam1=40, exam2=45, ability1="Clone", ability2="Summon", score=0, r=0}

sasuke::Ninja
sasuke = Ninja{name="Sasuke", country='f', status="Junior", exam1=50, exam2=60, ability1="Lightning", ability2="Fire",score=0, r=0}

gaara::Ninja
gaara=Ninja{name="Gaara", country='n', status="Junior", exam1=55, exam2=80, ability1="Vision", ability2="Sand",score=0, r=0}

temari::Ninja
temari=Ninja{name="Temari", country='n', status="Junior", exam1=40, exam2=60, ability1="Hit", ability2="Blade",score=0, r=0}


ninjas = [naruto, sasuke, gaara, temari]
 
fire::[Ninja]
fire = [ n | n <- ninjas, country n == 'f']


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
    | country winner == 'n' = updateScoreHelper winner wind


updateScoreHelper :: Ninja -> [Ninja] -> [Ninja]
updateScoreHelper winner country = result where
    result = [newNinja] ++ filter(\n -> name n /= name winner) country
    newNinja = winner { score = score winner + 10}

--updateScore naruto

deleteLoser :: Ninja -> [Ninja]
deleteLoser loser
	| country loser == 'f' =  filter(\n -> name n /= name loser) fire 
	| country loser == 'e' =  filter(\n -> name n /= name loser) earth 
	| country loser == 'w' =  filter(\n -> name n /= name loser) water
	| country loser == 'l' =  filter(\n -> name n /= name loser) lightning
	| country loser == 'n' =  filter(\n -> name n /= name loser) wind

 

















