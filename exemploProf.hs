import Data.List
import Text.ParserCombinators.Parsec

-- Declara termos
data Term = Atom String 
          | Variable Name
          | Predicate (String, [Term])
--
-- Atom :: String -> Term
-- Variable :: Name -> Term
-- Predicate :: (String, [Term]) -> Term
--

-- Declara nomes
type Name = String

-- Declara unificadores
type Unifier = [(Name, Term)]

--
-- Lê um átomo
--   atom: lowercase+
--
atom :: Parser Term
atom = do
  -- lower :: Parser Char
  -- many1 :: Parser a -> Parser [a]
  -- many1 lower :: Parser String
  -- Usamos a seta para extrair o valor carregado!
  name <- many1 lower
  -- name :: String
  -- return :: Monad m => a -> m a
  -- Atom :: String -> Term
  -- Atom name :: Term
  -- return $ Atom name :: Parser Term
  return $ Atom name

--
-- Lê uma variável
--   variable :: uppercase alnum*
--
variable :: Parser Term
variable = do
  -- upper :: Parser Char
  -- alphaNum :: Parser Char
  -- many :: Parser a -> Parser [a]
  head <- upper
  tail <- many alphaNum
  -- head :: Char
  -- tail :: String
  -- head:tail :: String
  return $ Variable (head:tail)

--
-- Criamos uma regra capaz de ler espaços em branco!
--
--   whitespace: " "*
--
whitespace :: Parser ()
whitespace = do
  -- Note que queremos ZERO ou mais, então usamos o many!
  many (char ' ')
  -- Agora que concluímos, retornamos um valor ()!
  return ()

--
-- Criamos uma regra para ler uma vírgula entre espaços!
--   comma: whitespace "," whitespace
--
comma :: Parser ()
comma = do
  try whitespace
  char ','
  try whitespace

--
-- Lê um predicado
--   predicate: lowercase* "(" term-list ")"
--
predicate :: Parser Term
predicate = do
  -- Primeiro queremos ler o nome da regra
  name <- many1 lower
  -- name :: String
  -- char :: Parser Char
  try whitespace -- Espaço em branco opcional!
  char '('
  try whitespace -- Espaço em branco opcional!
  -- sepBy :: Parser a -> Parser b -> Parser [a]
  subterms <- term `sepBy` comma
  -- subterms :: [Term]
  try whitespace -- Espaço em branco opcional!
  char ')'
  try whitespace -- Espaço em branco opcional!
  -- Retorna o resultado
  return $ Predicate (name, subterms)

--
-- Lê um termo qualquer!
--   term: predicate | atom | variable
--
term :: Parser Term
term = do
  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- try :: Parser a -> Parser a
  --
  -- O motivo do try a seguir:
  --   Se eu estiver parseando "x", ele não vai conseguir
  --   ler um predicado, pois não vai encontrar o "(", e
  --   não sabe que deveria ignorar o nome já lido (pois
  --   falhou na metade); para ignorá-lo, usamos o
  --   combinador try :)
  try predicate <|> atom <|> variable

--
-- Vamos marcar o que estamos tentando ler!
-- Queremos ler um termo, e apenas ele: após ele, queremos
-- que o texto acabe!
--
--   unit: term eof
--
-- Note que eof significa End Of File
--
unit :: Parser Term
unit = do
  t <- term
  -- eof :: Parser ()
  eof
  -- Agora que sabemos que não há mais entrada...
  return t

--
-- { X |-> Y }
--
test :: Unifier
test = [
         ("X", Variable "Y")
       ]

--
-- Função principal do sistema
--
main :: IO ()
main = do
  -- Solicita dois termos ao usuário
  putStrLn "Digite o primeiro termo:"
  -- Lê o valor digitado
  -- getLine :: IO String
  str1 <- getLine
  putStrLn "Digite o segundo termo:"
  str2 <- getLine
  -- str :: String
  case (parse unit "primeiro" str1, parse unit "segundo" str2) of
    (Right e1, Right e2) -> do
      -- Vamos tentar unificar os dois termos
      putStrLn "Unificacao:"
      print $ unify e1 e2
    (Left err, _) -> do
      print err
    (_, Left err) -> do
      print err

--
-- Ao invés de usarmos a conversão gerada para String,
-- com o deriving Show, vamos fazer uma na mão!
--
-- Quando chamamos a função print, ela usa essa função
-- show para transformar nosso objeto em String!
--
instance Show Term where
  show (Atom x) = -- (1)
    x
  show (Variable x) = -- (2)
    x
  show (Predicate (x, es)) = -- (3)
    -- es :: [Term]
    -- fmap :: (a -> b) -> [a] -> [b]
    -- intercalate :: [a] -> [[a]] -> [a]
    x ++ "(" ++ intercalate "," (fmap show es) ++ ")"

--
-- SUBSTITUIÇÃO: recebe um unificador/substituição s e
-- um termo e, e retorna o termo s(e)
--
subst :: Unifier -> Term -> Term

--
-- Átomos fazem parte da estrutura e nunca mudam; portanto:
--   s(x) = x
--
-- Ou seja: não importa quem é minha substituição, o átomo
-- continua o mesmo!
--
-- Exemplo:
--   { X |-> foo, Y |-> bar }(baz) = baz
--   { X |-> foo, Y |-> bar }(x) = x
--
subst s (Atom x) =
  Atom x

--
-- Variáveis devem ser trocadas pela substituição se, e
-- somente se, a substituição a conter em seu domínio:
--   s(X) = e      se s contém X |-> e,
--          X      do contrário
--
-- Exemplo:
--   { X |-> foo, Y |-> bar }(X) = foo
--   { X |-> foo, Y |-> bar }(Z) = Z
--
subst s (Variable x) =
  -- lookup :: Eq a => a -> [(a, b)] -> Maybe b
  -- lookup x s :: Maybe Term
  --
  -- Vamos agora verificar se X |-> e existe em s!
  --
  case lookup x s of
    -- Opa, existe!
    Just e ->
      -- Just :: a -> Maybe a
      -- e :: Term
      -- Como X |-> e existia, retornamos o termo e!
      e
    -- Não existe!
    Nothing ->
      -- Como X |-> e não existia, retornamos a variável X!
      Variable x

--
-- A aplicação da substituição em um predicado, que é um termo
-- composto, é dada preservando sua estrutura, aplicando a
-- substituição recursivamente em subtermos.
--    s(x(e1, ..., en)) = x(s(e1), ..., s(en))
--      ^^^^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^
--
-- O resultado da aplicação da substituição em um predicado sempre
-- resulta em um predicado!
--
-- Exemplo:
--   { X |-> foo, Y |-> bar }(pred(X, Y, Z)) = pred(foo, bar, Z)
--
subst s (Predicate (x, es)) =
  -- Se es = [e1, ..., en], queremos um termo que seja
  --   [subst s e1, ..., subst s en]
  Predicate (x, fmap (subst s) es)
  --                 ^^^^^^^^^
  -- Isso acima se chama uma APLICAÇÃO PARCIAL:
  -- Como:
  --   s :: Unifier
  --   subst :: Unifier -> (Term -> Term)
  -- Então
  --   subst s :: Term -> Term
  --   fmap (subst s) :: [Term] -> [Term]
  --
  -- Ao passarmos apenas o primeiro argumento, temos uma
  -- função que aguarda o segundo argumento!
  --

--
-- A função occursCheck é usada para verificar se uma
-- variável aparece (livre) em um term!
--
occursCheck :: Name -> Term -> Bool

--
-- Verificamos, por exemplo, se a variável X existe dentro
-- de um termo que é apenas o átomo y. Como um átomo NUNCA
-- é uma variável, não importa quem é X e quem é y, nunca
-- teremos que X aparece em y!
--
occursCheck x (Atom y) =
  False

--
-- Verificamos, por exemplo, se a variável X existe dentro
-- de um termo que é apenas a variável Y (para qualquer Y).
--
-- Por exemplo,
--   X existe dentro do termo X,
--   Mas X não existe dentro do termo Y!
--
-- Ou seja, precisamos verificar se são iguais!
--
occursCheck x (Variable y) =
  -- Se e somente se x for igual a y
  x == y

--
-- Finalmente, queremos verificar se uma variável X aparece
-- em um termo que é um predicado, no formato x(e1, ..., en).
--
-- Por exemplo,
--      X aparece em foo(X)
--      X aparece em bar(aaa, baz(X, bbb), ccc)
--      X não aparece em qux(Y, foo)
--
-- Ou seja, se X aparece em um dos subtermos, então X aparece
-- no predicado completo!
--
occursCheck x (Predicate (y, es)) =
  -- Lembrando que y é apenas o nome, não nos importamos com o
  -- que tem nele, ele não é uma variável
  --
  -- Sabemos que es = [e1, ..., en], e queremos saber se,
  -- para qualquer ei, occursCheck x ei é verdadeiro.
  --
  -- Podemos, por exemplo, usar um fmap para aplicar a função
  -- occursCheck x em cada elemento!
  --
  --   fmap (occursCheck x) es = [occursCheck x e1, ..., occursCheck x en]
  --
  -- Agora temos uma lista de valores booleanos! Podemos usar uma
  -- função de dobra agora para colocar uma operação entre cada valor! :)
  --
  let bs = fmap (occursCheck x) es in
  -- bs :: [Bool]
  foldr (||) False bs

--
-- Tentamos unificar dois termos: recebemos eles como
-- argumento, e talvez retornemos um unificador, através
-- do tipo Maybe
--
unify :: Term -> Term -> Maybe Unifier

--
-- Vamos tentar unificar duas variáveis iguais.
--
-- Regra:
--
--      --------------- (VAR)
--         X ~ X = {}
--
-- Note que só entramos nessa equação se temos duas variáveis,
-- X e Y, tal que X = Y!
--
unify (Variable x) (Variable y) | x == y = -- (1)
  -- Lembramos que usamos o Just para retornar um valor
  --   Just :: a -> Maybe a
  -- Como unificadores são listas de tuplas, um unificador
  -- vazio é apenas uma lista vazia!
  Just [] -- Unificador vazio!

--
-- Se temos uma variável na esquerda, ela pode virar o termo
-- na direita caso não apareça livre nele!
--
--       X not free in e
--   ----------------------- (LEFT)
--     X ~ e = { X |-> e }
--
unify (Variable x) e | not (occursCheck x e) = -- (2)
  -- Retornamos apenas { X |-> e }
  Just [(x, e)]

--
-- De forma similar à regra acima, mas para a direita.
--
--       X not free in e
--   ----------------------- (RIGHT)
--     e ~ X = { X |-> e }
--
unify e (Variable x) | not (occursCheck x e) = -- (3)
  -- Retornamos apenas { X |-> e }
  Just [(x, e)]

--
-- Dois átomos diferentes unificam!
--
--
--   -------------- (ATOM)
--     x ~ x = {}
--
unify (Atom x) (Atom y) | x == y = -- (4)
  -- Retorna o unificador vazio
  Just []

--
-- Para unificarmos dois predicados, precisamos garantir
-- que todos os subelementos unificam!
--
-- Para isso, precisamos de outra função que, ao invés de
-- unificar termos, unifica listas de termos! Repare que
-- o nome dos dois predicados também é igual!
--
--      [a1, ..., an] ~ [b1, ..., bn] = s
--   --------------------------------------- (PRED)
--     x(a1, ..., an) ~ x(b1, ..., bn) = s
--     ^                ^
--
unify (Predicate (x, as)) (Predicate (y, bs)) | x == y = -- (5)
  -- Note que, se unifyList as bs falhar, não poderemos unificar
  -- os predicados, portanto, podemos apenas reescrever para ele!
  unifyList as bs

--
-- Equação geral: para dois termos arbitrários, nós
-- NÃO podemos unificar
--
unify a b = -- (6)
  -- Não caiu nas equações acima, então não unifica!
  Nothing -- Não unifica!

--
-- Unificamos listas de termos!
--
unifyList :: [Term] -> [Term] -> Maybe Unifier

--
-- Listas vazias unificam, retornando a substituição vazia!
--
--   ---------------- (NIL)
--     [] ~ [] = {}
--
unifyList [] [] = -- (1)
  Just [] -- Unificador vazio

--
-- Unifica duas células.
--
--     a ~ b = s1      s1(as) ~ s1(bs) = s2
--   ---------------------------------------- (CONS)
--          (a:as) ~ (b:bs) = s2 * s1
--
-- Note que aplicar uma substituição em uma lista significa
-- aplicar essa substituição em cada elemento dela! Ou seja,
--
--   s[x, y, z] = [s(x), s(y), s(z)]
--
unifyList (a:as) (b:bs) =
  -- Verificamos a primeira hipótese!
  case unify a b of
    Just s1 ->
      -- Primeira hipótese retornou com sucesso!
      -- Vamos agora aplicar a substituição em ambas as caudas,
      -- dando um nome para as variáveis
      let as' = fmap (subst s1) as in
      let bs' = fmap (subst s1) bs in
      -- Agora, as' e bs' podem ser unificados? A segunda
      -- hipótese é verdadeira?
      case unifyList as' bs' of
        Just s2 ->
          -- Sucesso em ambas as hipóteses!
          -- Finalmente, nosso resultado exige compor as
          -- substituições
          Just $ compose s2 s1
        Nothing ->
          -- A segunda hipótese falhou, então essas células
          -- não podem ser unificadas!
          Nothing
    Nothing ->
      -- A primeira hipótese falhou, então essas células
      -- não podem ser unificadas!
      Nothing

--
-- Equação geral: listas de tamanho diferente não
-- unificam!
--
unifyList as bs = -- (3)
  -- Somente caímos aqui se as for vazio e bs não, ou
  -- vice-versa! As listas tinham tamanho diferente!
  Nothing

--
-- Agora queremos compor duas substituições!
--
-- Lembre-se que compor as substituições significa juntar
-- os elementos, porém aplicando as substituições da
-- esquerda em todos os elementos da direita!
--
-- Por exemplo:
--
--   { X |-> e1 } * { Y |-> e2 } =
--     { X |-> e1, Y -> { X |-> e1 }(e2) }
--
compose :: Unifier -> Unifier -> Unifier
compose s2 s1 =
  -- Ambos s2 e s1 são listas de tuplas!
  -- Lembrando que ++ faz a concatenação!
  -- Para alterar CADA elemento de s1, novamente, usamos a
  -- função fmap!
  s2 ++ fmap substElement s1
  where
    -- Como eu substituo um único elemento?
    substElement :: (Name, Term) -> (Name, Term)
    substElement (x, e) =
      -- Essa função irá ser usada em cada elemento de s1...
      -- O nome da variável permanece o mesmo, porém, o
      -- elemento e é trocado pela substituição s2(e)!
      (x, subst s2 e)
