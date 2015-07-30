-- A typeclass defines an interface that types in the typeclass must adhere to.
-- Here, we define the YesNo typeclass (which is NOT a type).
-- Types in the YesNo typeclass must define yesno, which takes a parameter of
-- the same type as the type in the typeclass and returns a Bool.

-- Create a custom typeclass that defines a function `yesno`.
-- Note that `a` in `class YesNo a` is the SAME `a` as `a -> Bool`!
class YesNo a where
  yesno :: a -> Bool

-- Define how `yesno` applies to integers
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

-- Define how `yesno` applies to lists
instance YesNo [a] where
  yesno [] = False
  yesno _ = True

-- Define how `yesno` applies to boolean values
instance YesNo Bool where
  -- Just use the boolean value itself
  yesno = id

instance YesNo (Maybe _) where
  yesno (Just _) = True
  yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesnoResult noResult =
    if (yesno yesnoVal)
    then yesnoResult
    else noResult
