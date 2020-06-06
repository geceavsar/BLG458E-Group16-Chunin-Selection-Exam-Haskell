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
naruto = Ninja{name="Naruto", country='F', status="Junior", exam1=40, exam2=45, ability1="Clone", ability2="Summon", score=0, r=0}

sasuke::Ninja
sasuke = Ninja{name="Sasuke", country='F', status="Junior", exam1=50, exam2=60, ability1="Lightning", ability2="Fire",score=0, r=0}

gaara::Ninja
gaara=Ninja{name="Gaara", country='W', status="Junior", exam1=55, exam2=80, ability1="Vision", ability2="Sand",score=0, r=0}

temari::Ninja
temari=Ninja{name="Temari", country='W', status="Junior", exam1=40, exam2=60, ability1="Hit", ability2="Blade",score=0, r=0}


ninjas = [naruto, sasuke, gaara, temari]
 
fire::[Ninja]
fire = [ n | n <- ninjas, country n == 'F']


wind::[Ninja]
wind=[ n | n <- ninjas, country n == 'W']
