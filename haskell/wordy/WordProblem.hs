{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordProblem (answer) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.Functor
import           Debug.Trace
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.ParserCombinators.ReadP    (char, endBy1, eof, get,
                                                  manyTill, munch, munch1,
                                                  satisfy, skipMany, skipSpaces,
                                                  string)
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadPrec hiding (get)


---------------------------------
-- Syntax tree and evaluation. --
---------------------------------

data Expr = Val Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
            deriving Show


eval ∷ Expr → Integer
eval (Val x  ) = x
eval (Sub x y) = eval x - eval y
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x × eval y
eval (Div x y) = eval x `div` eval y


----------------------------------
-- Parsing complex expressions. --
----------------------------------

-- | Parse and evaluate a word problem.
answer ∷ String → Maybe Integer
answer (fmap toLower → input)
    | null result = Nothing
    | otherwise   = let parse = (fst $ head result)
                    in Just (eval parse)

    where
      result  = readPrec_to_S runExpr 0 input
      opener  = lift (void $ string "what is")
      runExpr = do

        opener
        lift skipSpaces
        exp ← exprL
        optional (lift $ char '?')
        lift eof
        return exp


-- | This parses the word problem under the assumption that all operations
-- associate to the left.
exprL ∷ ReadPrec Expr
exprL = number +≫ do

          left    ← expr
          (_, op) ← operator
          right   ← number

          return $ op left right


-- | This parses the word problem using the usual precedence assumptions on
-- operations.
expr ∷ ReadPrec Expr
expr = number +≫ do

         -- This has to consume tokens instead of expressions first, since
         -- otherwise we would recur infinitely.
         left      ← some token
         (stp, op) ← operator
         right     ← some token

         -- Determine the appropriate action on precedence.
         let doStp = if stp > 0 then step else id

         -- Now that we’ve consumed an amount of tokens, we can feed the to the
         -- expr parser without infinite recursion.
         lExp  ← doStp $ embed expr (unwords left)
         rExp  ← doStp $ embed expr (unwords right)

         return (op lExp rExp)


----------------------------------
-- Parsing lexical expressions. --
----------------------------------

operator ∷ ReadPrec (Prec, Expr → Expr → Expr)
operator = add +≫ sub +≫ mul +≫ div

    where
      add = prec 0 ∘ lift $ asToken "plus"          ≫ return (0,Add)
      sub = prec 0 ∘ lift $ asToken "minus"         ≫ return (0,Sub)
      mul =          lift $ asToken "multiplied by" ≫ return (1,Mul)
      div =          lift $ asToken "divided by"    ≫ return (1,Div)


number ∷ ReadPrec Expr
number = do

  isNeg ← lift (char '-' ≫ return True) <++ return False
  numChars ← lift $ munch1 isNumber
  lift (skipSpaces +≫ eof +≫ void (char '?'))
  let num = if isNeg
            then negate (read numChars)
            else read numChars
  return $ Val num


token ∷ ReadPrec String
token = do

  tok ← lift $ munch1 (not ∘ isSpace)
  lift tokenSep
  return tok


tokenSep ∷ ReadP.ReadP ()
tokenSep = void $ many (satisfy isSpace) ≫ optional eof


asToken ∷ String → ReadP.ReadP String
asToken s = do

  result ← string s
  tokenSep
  return result


----------------
-- Utilities. --
----------------

-- | This runs a parser inside of another, and lift the result into the outer
-- parser.
embed ∷ Show α ⇒ ReadPrec α → String → ReadPrec α
embed parser input = readP_to_Prec $ \prec →

  let result = readPrec_to_S
                 (parser ≫= \result → lift eof ≫ return result)
                 prec input

  in if null result
     then ReadP.pfail
     else ReadP.choice $ fmap (return ∘ fst) result

