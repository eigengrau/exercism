{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Test where

import Prelude.Unicode
import Language.Haskell.TH
import Language.Haskell.TH.Quote


test ∷ QuasiQuoter
test = QuasiQuoter {
         quoteExp  = bla,
         quotePat  = undefined,
         quoteType = undefined,
         quoteDec  = undefined
       }

testE ∷ String → Q Exp
testE = return ∘ LitE ∘ StringL

bla ∷ String → Q Exp
bla s = do
  let n = mkName "x"
  a ← fmap NoBindS [| do { $(return $ VarP n) ← [1]; return $(return $ VarE n) } |]
  b ← fmap NoBindS [| return $(return $ VarE n) |]
  return (DoE [a, b])

test2 ∷ String → Q Exp
test2 s = do
  return (LetE [])

-- $(return $ DoE [BindS (VarP (mkName "x")) (ListE [LitE (IntegerL 1)]), NoBindS (AppE (VarE (mkName "return")) (VarE (mkName "x")))])

-- $(let { a = liftM NoBindS [|return x|]; b = liftM NoBindS [| do { x <- [1]; return x}  |] } in do { s <- a; s2 <- b; return (DoE [s2, s]) })
-- $([|let $(return p) = 1 in $(return . VarE $ mkName "x")|])

-- $(return $ LetE [ValD (VarP (mkName "x")) (NormalB (LitE (IntegerL 1))) [] ] (VarE (mkName "x")))
