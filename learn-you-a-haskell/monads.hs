-- If we have a fancy value and a function that takes a normal value but returns
-- a fancy value, how do we feed that fancy value into the function?
-- Voila: bind
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b

maybeString :: Maybe String
maybeString = Just "cool"

fancyFunction :: String -> Maybe String
fancyFunction (x:xs) = Just xs

fancify :: Maybe String
fancify = maybeString >>= fancyFunction

-- Still Nothing after passing it through because
-- Nothing >>= f is Nothing for all f
stillNothing :: Nothing
stillNothing = Nothing >>= fancyFunction  >>= fancyFunction
