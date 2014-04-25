module Utils where

-- UTILS
if' b x y = if b then x else y
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c x y = c >>= \z -> if z then x else y

toBool Nothing = False
toBool (Just b) = b