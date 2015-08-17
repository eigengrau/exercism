{-# LANGUAGE UnicodeSyntax #-}

module BankAccount where

import Control.Concurrent.MVar
import Control.Monad.Unicode
import Prelude.Unicode


type Currency = Integer
type Balance  = Currency

type BankAccount  = MVar BankAccountʹ
data BankAccountʹ = BankAccountʹ {
      getBalanceʹ ∷ Maybe Balance
    }


getBalance ∷ BankAccount → IO (Maybe Balance)
getBalance = fmap getBalanceʹ ∘ readMVar


incrementBalance ∷ BankAccount → Currency → IO (Maybe Balance)
incrementBalance account money = do
    oldAccount ← takeMVar account
    let oldBalance = getBalanceʹ oldAccount
        newBalance = fmap (+money) oldBalance
        newAcct = BankAccountʹ newBalance
    putMVar account newAcct
    return newBalance


openAccount ∷ IO BankAccount
openAccount = print "open" ≫ newMVar newAccount ≫= \acc →  print "opened" ≫ return acc
    where newAccount = BankAccountʹ (Just 0)


closeAccount ∷ BankAccount → IO ()
closeAccount account = print "close" ≫ takeMVar account ≫ putMVar account closedAccount ≫ print "closed"
    where closedAccount = BankAccountʹ Nothing
