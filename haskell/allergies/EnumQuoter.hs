{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

module EnumQuoter (deriveEnum) where

import           Language.Haskell.TH
import Prelude.Unicode

----------------------
-- The main quoter. --
----------------------

deriveEnum ∷ Name → [Integer] → Q [Dec]
deriveEnum name values = [d|
    instance Enum $(conT name) where

        fromEnum x = $(mkCaseFrom 'x name values)
        toEnum   x = $(mkCaseTo   'x name values)

        pred x = $(zipPred 'x name)
        succ x = $(zipSucc 'x name)
  |]


------------------
-- succ & pred. --
------------------

zipSucc ∷ Name →  Name → Q Exp
zipSucc var name = do
  constrNames ← listConstr name
  zipCase var constrNames (tail constrNames)

zipPred ∷ Name →  Name → Q Exp
zipPred var name = do
  constrNames ← listConstr name
  zipCase var (tail constrNames) constrNames

zipCase ∷ Name → [Name] → [Name] → Q Exp
zipCase var from to  = do
  errMatch ← errorMatch
  let matches = map caseMatch pairs ⧺ [errMatch]
  return (CaseE (VarE var) matches)

    where
      pairs = zip from to
      caseMatch ∷ (Name, Name) → Match
      caseMatch (a, b) = Match (ConP a []) (NormalB $ ConE b) []


------------------------
-- fromEnum & toEnum. --
------------------------

mkCaseFrom ∷ Name → Name → [Integer] → Q Exp
mkCaseFrom var name vals = do
    constrNames ← listConstr name
    mkCaseʹ var constrNames vals
        where
          mkCaseʹ ∷ Name → [Name] → [Integer] → Q Exp
          mkCaseʹ var names vals = return (CaseE (VarE var) matches)
              where matches = caseMatches names vals

          caseMatches ∷ [Name] → [Integer] → [Match]
          caseMatches names vals = zipWith mkMatch names vals
              where
                mkMatch conName val = let pattern = ConP conName []
                                          body    = NormalB (LitE $ IntegerL val)
                                      in Match pattern body []

mkCaseTo ∷ Name → Name → [Integer] → Q Exp
mkCaseTo vName name vals = do
  constrNames ← listConstr name
  mkCase2ʹ vName vals constrNames
      where
        mkCase2ʹ ∷ Name → [Integer] → [Name] → Q Exp
        mkCase2ʹ var vals names = do
                             err ← errorMatch
                             return (CaseE (VarE var) (matches ⧺ [err]))
            where matches = caseMatches2 vals names

        caseMatches2 ∷ [Integer] → [Name] → [Match]
        caseMatches2 vals names = zipWith mkMatch vals names
            where
              mkMatch val conName = let pattern = LitP (IntegerL val)
                                        body    = NormalB (ConE conName)
                                    in Match pattern body []


----------------
-- Utilities. --
----------------

listConstr ∷ Name → Q [Name]
listConstr name = do
  TyConI (DataD _ _ _ constructors _) ← reify name
  let names = map (\(NormalC x _) → x) constructors
  return names


errorMatch ∷ Q Match
errorMatch = do
  var ← newName "x"
  body ← mkBody
  return (Match (VarP var) body [])
    where
      mkBody = ([|error "hi"|]) >>= \code → return $ NormalB code
