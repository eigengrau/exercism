{-# LANGUAGE UnicodeSyntax #-}

module BankAccount (
    BankAccount,
    openAccount,
    closeAccount,
    getBalance,
    incrementBalance
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.Unicode
import           Prelude.Unicode


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
        newAccount = BankAccountʹ newBalance
    putMVar account newAccount
    return newBalance


openAccount ∷ IO BankAccount
openAccount = newMVar newAccount
    where newAccount = BankAccountʹ (Just 0)


closeAccount ∷ BankAccount → IO ()
closeAccount account = takeMVar account ≫ putMVar account closedAccount
    where closedAccount = BankAccountʹ Nothing
