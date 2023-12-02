module Evaluation(evaluate) where

import Data.Function((&))
import Control.Monad.Reader
import Data.Map(Map, member, (!))
import Grammar
import BetaReduction

containsContextVariable :: Expression -> Reader Context Bool
containsContextVariable (Var v) =
  do
    ctx <- ask
    return $ v `member` ctx
-- for the abs case, to be 100% correct we should delete the bound variable from the context
-- but it's not necessary because context variables are free in the expression and we always
-- rename all the non free variables to not match any of the free ones or eachother
containsContextVariable (Abs _ e) = containsContextVariable e
containsContextVariable (App l r) =
  do
    lContainsContextVariable <- containsContextVariable l
    rContainsContextVariable <- containsContextVariable r
    return $ lContainsContextVariable || rContainsContextVariable

-- for the abs case, to be 100% correct we should delete the bound variable from the context
-- but it's not necessary because context variables are free in the expression and we always
-- rename all the non free variables to not match any of the free ones or eachother
useContextOneStep :: Expression -> Reader Context Expression
useContextOneStep (Var v) =
  do
    ctx <- ask
    return $ if v `member` ctx then ctx ! v else Var v
useContextOneStep (Abs v e) = (Abs v) <$> useContextOneStep e
useContextOneStep (App l r) =
  do
    ccvl <- containsContextVariable l
    if ccvl then
      App <$> useContextOneStep l <*> return r
    else
      App <$> return l <*> useContextOneStep r

evaluateOneStep :: Expression -> Reader Context Expression
evaluateOneStep e =
  if containsReducibleAbstraction e then
     return . betaReduceOneStep $ e
  else 
     useContextOneStep e

evaluate' :: Expression -> Reader Context Expression
evaluate' e =
  do
    let cra = containsReducibleAbstraction e
    crv    <- containsContextVariable e
    if cra || crv then
      e & return . makeBoundVariablesUnique >>= evaluateOneStep >>= evaluate'
    else
      return e 

evaluate :: Program -> Expression
evaluate (c, e) = runReader (evaluate' e) c
