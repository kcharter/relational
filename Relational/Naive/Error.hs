module Relational.Naive.Error (die) where

import Control.Monad.Error (Error, MonadError, throwError, strMsg)

die :: (Error e, MonadError e m) => String -> m a
die = throwError . strMsg

