module MyTypeClass where

-- Create a custom typeclass that defines a function `yesno
class YesNo a where
  yesno :: a -> Bool

-- Define how `yesno` applies to Ints
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

-- Define how `yesno` applies to arrays
instance YesNo [a] where
  yesno [] = False
  yesno _ = True

-- Define how `yesno` applies to boolean values
instance YesNo Bool where
  -- Just use the boolean value itself
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesnoResult noResult = if (yesno yesnoVal) then yesnoResult else noResult
