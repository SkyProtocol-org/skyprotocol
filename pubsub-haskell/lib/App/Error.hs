module App.Error where

data AppError
  = HandlerError String
  | AppError String
  deriving (Show)
