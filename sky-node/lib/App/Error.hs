module App.Error where

-- | Top-level error type
data AppError
  = -- | Constructor for error propagation from the handler level. Used for errors that must terminate the execution of the node.
    HandlerError String
  | -- | Constructor for errors in the node itself.
    AppError String
  deriving (Show)
