module BetaReduction(betaReduce, betaReduceOneStep, containsReducibleAbstraction) where

import Grammar

substitute :: String -> Expression -> Expression -> Expression
substitute toBeReplaced toBeReplacedWith toReplaceIn =
  case toReplaceIn of
    Var v   -> if v == toBeReplaced then toBeReplacedWith else toReplaceIn
    Abs v e -> if v == toBeReplaced then s e else Abs v $ s e
    App l r -> App (s l) (s r)
  where s = substitute toBeReplaced toBeReplacedWith

containsReducibleAbstraction :: Expression -> Bool
containsReducibleAbstraction (Var _) = False
containsReducibleAbstraction (Abs _ e) = containsReducibleAbstraction e
containsReducibleAbstraction (App (Abs _ _) r) = True
containsReducibleAbstraction (App l r) = containsReducibleAbstraction l || containsReducibleAbstraction r

betaReduceOneStep :: Expression -> Expression
betaReduceOneStep (App (Abs v e) r) = substitute v r e
betaReduceOneStep (App l r) = if containsReducibleAbstraction l then App (betaReduceOneStep l) r else App l (betaReduceOneStep r)
betaReduceOneStep (Abs v e) = Abs v $ betaReduceOneStep e
betaReduceOneStep e = e

betaReduce :: Expression -> Expression
betaReduce e
  | containsReducibleAbstraction e = betaReduce . betaReduceOneStep . makeBoundVariablesUnique $ e
  | otherwise = e
