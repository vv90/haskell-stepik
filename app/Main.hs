module Main where

import Lib
import Control.Monad (guard, msum, when, unless)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.Char (toUpper, isNumber, isPunctuation)
import Data.Either (isRight)

import ExceptTRead

secondElem :: Reader [String] String
secondElem = asks $ fmap toUpper . head . tail

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

data PwdError = PwdError String

instance Monoid PwdError where
    mempty = PwdError mempty

instance Semigroup PwdError where
    PwdError s1 <> PwdError s2 = PwdError $ s1 <> s2



type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
    liftIO $ putStrLn "Enter your new password:"
    value <- msum $ repeat getValidPassword
    liftIO $ putStrLn "Storing in database..."
      

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine 
    case validatePassword s of
        Right validPassword -> do
            pure s
        Left (PwdError err) -> do
            liftIO $ putStrLn err
            throwE $ PwdError err


validatePassword :: String -> Either PwdError String
validatePassword pwd
    | length pwd <= 8 = Left $ PwdError "Incorrect input: password is too short!"
    | not $ any isNumber pwd = Left $ PwdError "Incorrect input: password must contain some digits!"
    | not $ any isPunctuation pwd = Left $ PwdError "Incorrect input: password must contain some punctuation!"
    | otherwise = Right pwd

main :: IO ()
main = do
    putStrLn "here we go..."
    putStrLn "here we go again..."
