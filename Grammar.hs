module Grammar (Expression(..), freeVariables, Definition, Context, Program, makeBoundVariablesUnique) where 

import Data.List(union)
import Data.Map(Map)
import Control.Monad.State.Lazy

data Expression = Var String | Abs String Expression | App Expression Expression

freeVariables :: Expression -> [String]
freeVariables (Var v) = [v]
freeVariables (Abs v e) = filter (/= v) $ freeVariables e 
freeVariables (App e1 e2) = union (freeVariables e1) (freeVariables e2)

replaceVariableUnlessBound :: String -> String -> Expression -> Expression
replaceVariableUnlessBound old new e =
  case e of
    Var v   -> Var $ if v == old then new else v
    Abs v e -> if v == old then e else Abs v (r' e)
    App l r -> App (r' l) (r' r)
  where r' = replaceVariableUnlessBound old new

nextUnusedVariableName :: State [String] String
nextUnusedVariableName = 
  do
    used <- get
    let next = head . dropWhile (`elem` used) $ allVariableStrings
    put (next:used)
    return next
  where allVariableStrings = ['_':y:x | x <- "":allVariableStrings, y <- ['a'..'z']]

makeBoundVariablesUnique' :: Expression -> State [String] Expression
makeBoundVariablesUnique' (App e1 e2) = App <$> makeBoundVariablesUnique' e1 <*> makeBoundVariablesUnique' e2 
makeBoundVariablesUnique' (Abs v e) =
  do
    newV <- nextUnusedVariableName
    newE <- makeBoundVariablesUnique' $ replaceVariableUnlessBound v newV e
    return $ Abs newV newE
makeBoundVariablesUnique' v = return v

makeBoundVariablesUnique :: Expression -> Expression
makeBoundVariablesUnique e = fst $ runState (makeBoundVariablesUnique' e) (freeVariables e)

instance Show Expression where
  show (Var v) = v
  show (Abs v e) = "(λ" ++ v ++ "." ++ show e ++ ")"
  show (App l r) = "(" ++ show l ++ show r ++ ")"

literalEq :: Expression -> Expression -> Bool
literalEq (Var v1) (Var v2) = v1 == v2
literalEq (Abs v1 e1) (Abs v2 e2) = v1 == v2 && (literalEq e1 e2)
literalEq (App l1 r1) (App l2 r2) = (literalEq l1 l2) && (literalEq r1 r2)
literalEq e1 e2 = False

instance Eq Expression where
 (==) e1 e2 = 
   freeVariables e1 == freeVariables e2 &&
   literalEq (makeBoundVariablesUnique e1) (makeBoundVariablesUnique e2)

type Definition = (String, Expression)
type Context = Map String Expression
type Program = (Context, Expression)
