1'  Q mport Text.ParserCombinators.Parsec

data Type = TypeInt
          | TypeVar Name
          | TypeArrow Type Type
          deriving Show
                                                
type Name = String

type Unifier = [(Name, Type)]

typeInt :: Parser Type
typeInt = do
    strt <-  char 'i'
    mdl <-  char 'n'
    nd <-  char 't'
    return $ TypeInt (strt:mdl:nd)  

typeVar :: Parser Type
typeVar = do
    name <- many1 lower
    return $ TypeVar name

typeArrow :: Parser Type
typeArrow = do
    tipo <- many1 lower
    "->"
    tipo <- many1 lower
    return $ TypeArrow name nome

