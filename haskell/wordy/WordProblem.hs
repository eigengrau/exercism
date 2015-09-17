{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module WordProblem (answer) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Char
import           Data.Monoid
import           Prelude.Unicode
import           Prelude.Unicode.SR
import           Text.ParserCombinators.ReadP    (char, eof, munch1, satisfy,
                                                  skipSpaces, string)
import qualified Text.ParserCombinators.ReadP    as ReadP
import           Text.ParserCombinators.ReadPrec hiding (get)


                           -- In addition to parsing only left-associatively,
                           -- I’ve also added a precedence-based parser which
                           -- binds multiplication and addition more tightly.
                           --
                           -- I thought before going for Parsec, I might try the
                           -- parsing combinators included with base first.
                           -- `ReadP` is decent by itself, and Hutton-style
                           -- monadic parser combinators are nice, but the
                           -- precedence-based version `ReadPrec` turned out to
                           -- be a bit awkward. While I would have expected the
                           -- precedence context to be maintained monadically,
                           -- so that one would write `step n ≫ parseAction`,
                           -- `step` is just a combinator instead, so that
                           -- downstream parsers have to pass up the expected
                           -- precedence levels manually.
                           --
                           -- Additionally, the precedence-based version has to
                           -- do look-ahead on the input, while ReadP & co.
                           -- provide no ready-made versions to perform
                           -- conditional read-ahead based on self-defined
                           -- parsers. Instead, it seems one has to consume
                           -- characters and run the desired parser in an
                           -- embedded fashion (for which `ReadP` doesn’t seem
                           -- to provide any pre-defined means).


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

-- | Parse and evaluate a word problem using the default parser.
answer ∷ String → Maybe Integer
answer = answerWith exprL


-- | Parse and evaluate a word problem using the provided parser.
answerWith ∷ ReadPrec Expr → String → Maybe Integer
answerWith parser (fmap toLower → input)
    | null result = Nothing
    | otherwise   = let parse = fst (head result)
                    in Just (eval parse)

    where
      result  = readPrec_to_S runExpr 0 input
      opener  = lift ∘ void $ string "what is"
      runExpr = do

        opener
        lift skipSpaces
        expr ← parser
        optional ∘ lift $ char '?'
        lift eof
        return expr


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

         -- This has to do look-head for an operator, to ensure that parsing the
         -- left and right context will termitate.
         left       ← some token
         (prec, op) ← operator
         right      ← some token

         -- Set how the precedence context will be increased based on the
         -- precedence the operator demands.
         let stepUp = appEndo $ foldMap Endo (replicate prec step)

         -- Recurse into the left and right context. Since we’ve already
         -- consumed the characters, we must «embed» the expression parser.
         lExp  ← stepUp $ embed expr (unwords left)
         rExp  ← stepUp $ embed expr (unwords right)

         return (op lExp rExp)


----------------------------------
-- Parsing lexical expressions. --
----------------------------------

operator ∷ ReadPrec (Prec, Expr → Expr → Expr)
operator = msum [add, sub, mul, div]

    where
      -- Addition and subtraction are blocked when a multiplication/division
      -- expression is being parsed.
      add = prec 0 ∘ lift $ asToken "plus"          ≫ return (0,Add)
      sub = prec 0 ∘ lift $ asToken "minus"         ≫ return (0,Sub)
      mul =          lift $ asToken "multiplied by" ≫ return (1,Mul)
      div =          lift $ asToken "divided by"    ≫ return (1,Div)


number ∷ ReadPrec Expr
number = do

  isNeg    ← lift (char '-' ≫ return True) <⧺ return False
  numChars ← lift $ munch1 isNumber

  lift $ skipSpaces +≫ eof +≫ void (char '?')

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


(<⧺) ∷ ReadPrec α → ReadPrec α → ReadPrec α
(<⧺) = (<++)
