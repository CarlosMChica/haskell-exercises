{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Exercises where

import Data.Foldable (fold)
import Data.Semigroup
import Data.Maybe
import Debug.Trace


{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil  :: CountableList
  CountableCons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList CountableNil         = 0
countList (CountableCons x xs) = count x + countList xs


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero CountableNil = CountableNil
dropZero (CountableCons x xs) =
  if count x == 0
  then dropZero xs
  else CountableCons x (dropZero xs)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"

-- filterInts can't be impleted because there isn't any information available about the types of the elements. Hence we can't pattern match on them.

{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: Show a => a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList (AnyCons x xs) = reverseAnyList xs `anyConcat` AnyCons x AnyNil

anyConcat :: AnyList -> AnyList -> AnyList
anyConcat AnyNil b = b
anyConcat (AnyCons x xs) b = AnyCons x (anyConcat xs b)

-- filterAnyList :: (a -> Bool) -> AnyList -> AnyList
-- filterAnyList _ AnyNil = AnyNil
-- filterAnyList f (AnyCons x xs) = if f x == True then AnyCons x (filterAnyList f xs) else filterAnyList f xs

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0
lengthAnyList (AnyCons _ xs) = 1 + lengthAnyList xs

-- foldAnyList :: Monoid m => AnyList -> m
-- foldAnyList AnyNil = mempty
-- foldAnyList (AnyCons x xs) = x <> foldAnyList xs

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True
isEmptyAnyList _ = False

instance Show AnyList where
  show AnyNil = "[]"
  show (AnyCons x xs) = show x <> " : " <> show xs

{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?

-- The type variable input is exstential and we can only transform it to the output type variable

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq o => Eq (TransformableTo o) where
  (TransformWith f x) == (TransformWith g y) = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
  fmap f (TransformWith g x) = TransformWith (f . g) x

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

isEq :: EqPair -> EqPair -> Bool
isEq (EqPair x y) (EqPair x' y') = x == y && x' == y'

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqADT a = Eq a => EqADT a a

instance Eq (EqADT a) where
  (==) = isEq'

isEq' :: EqADT a -> EqADT a -> Bool
isEq' (EqADT x y) (EqADT x' y') = x == y && x' == y'

{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                              MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox x _) = x

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox x _)) = x

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers EmptyBox = 0
countLayers (IntBox _ s) = 1 + countLayers s
countLayers (StringBox _ s) = 1 + countLayers s
countLayers (BoolBox _ s) = 1 + countLayers s

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- • Could not deduce: b ~ () from the context: a ~ Int
-- removeLayer :: MysteryBox a -> MysteryBox b
-- removeLayer EmptyBox = EmptyBox
-- removeLayer (IntBox _ b) = b
-- removeLayer (StringBox _ b) =  b

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)


exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

hListHead :: HList (a, b) -> a
hListHead (HCons x _) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = undefined

-- patternMatchMe :: HList ((Int, String, Bool), ()) -> Int
-- patternMatchMe (HCons (uuu, _, _) _) = uuu

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

--notgoingtohappen :: HList (a, b) -> HList (c, d) -> HList (a, (b, (c, d))
--notgoingtohappen HNil HNil = HNil

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty  :: HTree ()
  HBranch :: HTree left -> center -> HTree right -> HTree (left, center, right)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

exampleHTree :: HTree (((), Integer, ()), Integer, ())
exampleHTree = HBranch (HBranch HEmpty 3 HEmpty) 2 HEmpty

exampleHTree' :: HTree (((), Integer, ()), Integer, ((), Integer, ()))
exampleHTree' = HBranch (HBranch HEmpty 1 HEmpty) 2 (HBranch HEmpty 3 HEmpty)

exampleHTree'' :: HTree (((), Integer, ()), Integer, ())
exampleHTree'' = HBranch (HBranch HEmpty 1 HEmpty) 2 HEmpty

exampleHTree''' :: HTree (((), Integer, ()), Integer, ())
exampleHTree''' = HBranch (HBranch HEmpty 1 HEmpty) 2 HEmpty

rmLeft :: HTree (a, b, c) -> HTree ((), b, c)
rmLeft (HBranch _ b c) = HBranch HEmpty b c

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. Recursion is your friend here - you
-- shouldn't need to add a constraint to the GADT!

instance Eq (HTree ()) where
  HEmpty == HEmpty = True

instance (Eq b, Eq (HTree a), Eq (HTree c)) => Eq (HTree (a, b, c)) where
  (HBranch l x r) == (HBranch l' x' r') = x == x' && l == l' && r == r'

{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil  :: AlternatingList c d
  ACons :: a -> AlternatingList b a -> AlternatingList a b

f :: AlternatingList Bool Int
f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))

f' :: AlternatingList Bool Int
f' = ACons True (ACons 1 (ACons False ANil))

f'' :: AlternatingList (All) (Sum Int)
f'' = ACons (All True) (ACons (Sum 1) (ACons (All False)(ACons (Sum 2) ANil)))

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts ANil                    = []
getFirsts (ACons x ANil)          = [x]
getFirsts (ACons x (ACons _ xs)) = x : getFirsts xs

getSeconds :: AlternatingList a b -> [b]
getSeconds ANil                    = []
getSeconds (ACons _ ANil)          = []
getSeconds (ACons _ (ACons x' xs)) = x' : getSeconds xs

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues xs = (fold ys, fold zs)
  where
    ys = getFirsts xs
    zs = getSeconds xs

foldValues' :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues' ANil = (mempty, mempty)
foldValues' (ACons x ANil) = (x, mempty)
foldValues' (ACons x (ACons x' xs)) = (x <> (fst $ foldValues' xs), x' <> (snd $ foldValues' xs))

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int           -> Expr Bool
  Add       :: Expr Int  -> Expr Int           -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a -> Expr a
  IntValue  :: Int                             -> Expr Int
  BoolValue :: Bool                            -> Expr Bool
  Function  :: (a -> Expr b) -> Expr (a -> b)
  Apply     :: Expr (a -> b) -> Expr a -> Expr b

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (BoolValue x)        = x
eval (IntValue x)         = x
eval (If cond expr expr') = if eval cond then eval expr else eval expr'
eval (Add x y)            = eval x + eval y
eval (Equals x y)         = eval x == eval y
eval (Apply f x)          = eval f $ eval x
eval (Function f)         = eval . f

--exampleExpr = Function (Equals (IntValue 1)) (IntValue 1)
exampleExpr = Apply (Function (\x -> IntValue $ x + 1)) (IntValue 1)

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool
  | DirtyFunction  (DirtyExpr -> DirtyExpr) DirtyExpr

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyIntValue x)        = Just $ IntValue x
parse (DirtyIf cond exp exp')  = If <$> parse' cond <*> parse exp <*> parse exp'
parse (DirtyAdd exp exp')      = Add <$> parse exp <*> parse exp'
-- parse (DirtyFunction f arg)    =
--   (Function $ fromJust . parse . f . (const $ fromJust . parse $ arg)) <$> parse arg
parse _                        = Nothing

parse' :: DirtyExpr -> Maybe (Expr Bool)
parse' (DirtyEquals exp exp') = Equals <$> parse exp <*> parse exp'
parse' (DirtyBoolValue x)     = Just $ BoolValue x
parse' _                      = Nothing

--exampleExpr' = eval . fromJust . parse $ DirtyFunction (DirtyEquals (DirtyIntValue 2)) (DirtyIntValue 1)

exampleAST :: DirtyExpr
exampleAST = DirtyIf (DirtyEquals (DirtyIntValue 1) (DirtyIntValue 1))
               (DirtyAdd (DirtyIntValue 5) (DirtyIntValue 10))
               (DirtyIntValue 1)

exampleAST' :: DirtyExpr
exampleAST' =
  DirtyIf
   (DirtyIf (DirtyEquals (DirtyIntValue 1) (DirtyIntValue 1))
               (DirtyBoolValue True)
               (DirtyBoolValue False))
  (DirtyIntValue 1)
  (DirtyIntValue 2)

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe'?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TNil :: TypeAlignedList b b
  TCons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs t TNil = t
composeTALs t  (TCons f TNil) = TCons f (composeTALs t TNil)
composeTALs t' (TCons f (TCons g t)) = TCons (g . f) (composeTALs t' t)

exampleTAL :: TypeAlignedList [Maybe Int] String
exampleTAL = TCons show TNil `composeTALs` TCons catMaybes (TCons head TNil)

-- evalTAL :: Monoid b => TypeAlignedList a b -> a -> b
-- evalTAL TNil a = mempty
-- evalTAL (TCons f TNil) a = f a
-- --evalTAL (TCons f (TCons g t)) a = (g . f) a <> evalTAL t a
-- evalTAL (TCons f (TCons g t)) a = g . f $ a

-- evalTAL :: Monoid b => TypeAlignedList a b -> a -> b
-- evalTAL t a = go t id $ a
--   where
--     go :: TypeAlignedList a b -> (forall a . (a -> b)) -> (a -> b)
--     go TNil f = f
--     go (TCons f TNil) g = f . g
--    go (TCons f (TCons g t)) h = go t (g . f . h)
