{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE UnicodeSyntax   #-}

module EnumQuoter (deriveEnum, EnumSafe(..)) where

import           Control.Applicative
import           Control.Applicative.Unicode
import           Data.Maybe
import           Language.Haskell.TH
import           Prelude.Unicode
import           Prelude.Unicode.SR


--------------
-- Classes. --
--------------

-- | A class for partial Enums.
class EnumSafe α where
    succMay    ∷ α → Maybe α
    predMay    ∷ α → Maybe α
    toEnumMay  ∷ Integral β ⇒ β → Maybe α
    fromEnumʹ  ∷ α → Integer


----------------------
-- The main quoter. --
----------------------

-- | Derive an Enum instance that follows a given pattern.
deriveEnum ∷ Integral α
           ⇒ Name         -- ^ The type for which the instance is to be derived.
           → [α]          -- ^ The values which enumerate the type, following the
                          --   sequence in which the constructors are declared.
           → Q [Dec]

deriveEnum name (map fromIntegral → values) = do

  constructors ← listConstructors name
  let minCon = head constructors
      maxCon = last constructors

  [d|
    instance EnumSafe $(conT name) where

        fromEnumʹ x = $(mapCases 'x conPʹ integerE Nothing constructors values)
        toEnumMay x = $(mapCases 'x integerP justE catchAll values constructors)

        predMay x = $(mapCases 'x conPʹ justE catchAll (tail constructors) constructors)
        succMay x = $(mapCases 'x conPʹ justE catchAll constructors (tail constructors))

    instance Enum $(conT name) where

        fromEnum = fromIntegral ∘ fromEnumʹ
        toEnum   = fromJust ∘ toEnumMay ∘ fromIntegral

        succ = fromJust ∘ succMay
        pred = fromJust ∘ predMay

        -- Since this is for Enums which may have gaps, the default
        -- implementation won’t do.
        enumFromTo $(conP maxCon []) _ = return $(conE maxCon)
        enumFromTo _ $(conP minCon []) = []
        enumFromTo x y                 = x : enumFromTo (succ x) y

   |]

    where
      integerE = LitE ∘ IntegerL
      integerP = LitP ∘ IntegerL
      conPʹ x  = ConP x []
      justE x  = AppE (ConE 'Just) (ConE x)
      catchAll = Just $ ConE 'Nothing


----------------
-- Utilities. --
----------------

-- Create a case expression that maps safely between cases.
mapCases ∷ Name        -- ^ Variable to bind.
         → (α → Pat)   -- ^ Derive the pattern matches.
         → (β → Exp)   -- ^ Deriving the returned expressions.
         → Maybe Exp   -- ^ Optional catch-all clause.
         → [α] → [β] → Q Exp

mapCases varName toPat toExp catchAll (ZipList → as) (ZipList → bs) =
    return $ CaseE (VarE varName) (matches ⧺ otherwiseʹ)

        where
          matches     = getZipList $ mkMatch ⦷ as ⊛ bs
          otherwiseʹ  = maybe [] (\body → return (Match WildP (NormalB body) [])) catchAll
          mkMatch a b = Match (toPat a) (NormalB $ toExp b) guards
          guards      = []


listConstructors ∷ Name → Q [Name]
listConstructors name = do

  TyConI (DataD _ _ _ constructors _) ← reify name
  let names = map (\(NormalC x _) → x) constructors
  return names
