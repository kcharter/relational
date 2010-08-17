module MonadUtil where

untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM test m =
    do x <- m
       if test x then return x else untilM test m