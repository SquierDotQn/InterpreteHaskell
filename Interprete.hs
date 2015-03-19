-- Théo Plockyn
-- TP5, interprete
import Parser
import Data.Maybe
import System.IO

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show, Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show, Eq)

--Q1
espacesP :: Parser ()
espacesP = (zeroOuPlus (car ' ') >>= \_ -> return ()) ||| (return ())

--Q2
nomP :: Parser Nom
nomP = nomP'    >>= \r ->
       espacesP >>= \_ ->
       return r
         where nomP' = unOuPlus (carCond (flip elem ['a'..'z']))
	
--Q3
varP :: Parser Expression
varP = nomP     >>= \r ->
       return (Var r)

--Q4
applique :: [Expression] -> Expression
applique = (foldl1 (\x -> App x ))

--Q5
exprP :: Parser Expression
exprP = exprParentheseeP ||| lambdaP ||| varP ||| nombreP ||| booleenP

exprsP :: Parser Expression
exprsP = unOuPlus exprP >>= \p ->
         return (applique p)

--Q6
lambdaP :: Parser Expression
lambdaP = car '\\'     >>= \_ ->
          espacesP     >>= \_ ->
          nomP         >>= \v ->
          chaine "->"  >>= \_ ->
          espacesP     >>= \_ ->
          exprsP       >>= \a ->
          return       (Lam v a)

--Q7
--Voir Q5

--Q8
exprParentheseeP :: Parser Expression
exprParentheseeP = (car '('          >>= \_ -> 
                    espacesP         >>= \_ ->
                    exprsP           >>= \e ->
                    car ')'          >>= \_ ->
                    espacesP         >>= \_ ->
                    return           e)
                    
--Q9
isChiffre :: Char -> Bool
isChiffre = flip elem ['0'..'9']

nombreP :: Parser Expression
nombreP = unOuPlus (carCond isChiffre) >>= \n ->
          espacesP                     >>= \_ ->
          return (Lit (Entier (read n)))

--Q10
booleenP :: Parser Expression
booleenP = (chaine "True"             >>= \_ ->
            espacesP                  >>= \_ ->
            return (Lit (Bool True))) |||
           (chaine "False"            >>= \_ ->
            espacesP                  >>= \_ ->
            return (Lit (Bool False)))

--Q11
expressionP :: Parser Expression
expressionP = espacesP                >>= \_ ->
              exprsP                  >>= \r ->
              return r

--Q12
ras :: String -> Expression
ras s = if complet  ( parse expressionP s ) then
           resultat ( parse expressionP s ) 
        else
           error "Erreur d'analyse syntaxique"

--Q13
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

--Q14
instance Show ValeurA where
    show (VFonctionA _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralA (Entier e)) = show e
    show (VLitteralA (Bool   b)) = show b

type Environnement a = [(Nom, a)] 

--Q15
interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA env (Var x) = fromJust (lookup x env)
interpreteA env (Lit x) = VLitteralA x
interpreteA env (Lam nom expr) = VFonctionA (\v -> interpreteA ((nom,v):env) expr)
interpreteA env (App a b)      = f x
				where {VFonctionA f = interpreteA env a;
				                  x = interpreteA env b}

--Q16
negA :: ValeurA
negA = VFonctionA (\(VLitteralA (Entier n)) -> (VLitteralA (Entier (-n))))

--Q17
addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier n)) -> VFonctionA (\(VLitteralA (Entier m)) ->  VLitteralA (Entier (n+m))))

--Q18
envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if"  ,  ifthenelseA) ]

releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA f = VFonctionA (\(VLitteralA (Entier n)) -> VFonctionA (\(VLitteralA (Entier m)) -> VLitteralA (Entier (f n m) ) ) )

--Q19
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool i)) -> VFonctionA (\t -> VFonctionA ( \e -> if i then t else e ) ) )

--Q20
main :: IO ()
main = do{
	putStr ">";
	l   <- getLine;
--	eof <- isEOF
--	if eof then
--		putStrLn "fini"
--	else
--		do
			print (interpreteA envA (ras l));
			main}
--eof  <- isEOF
--       unless eof main
