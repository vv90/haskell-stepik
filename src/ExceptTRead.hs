module ExceptTRead where

-- import Control.Monad.Trans.Except

data ReadError = EmptyInput | NoParse String
  deriving Show

-- tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a