module App.Error where

-- | Top-level error type
data AppError
  = -- | Constructor for error propagation from the handler level.
    HandlerError String
  | -- | Constructor for errors in the node itself.
    AppError String
  deriving (Show)
