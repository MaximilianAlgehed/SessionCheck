{-# LANGUAGE TypeOperators
           , FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses #-}
module SessionCheck.Predicate where

import Test.QuickCheck
import Data.List
import Data.Char

import SessionCheck.Classes

-- A representation of dualizable predicates 
data Predicate a =
  Predicate { apply     :: a -> Bool
            , satisfies :: Gen a
            , shrunk    :: a -> Gen a
            , name      :: String }

bimap :: (a -> b) -> (b -> a) -> Predicate a -> Predicate b
bimap toB toA p = p { apply     = apply p . toA
                    , satisfies = toB <$> satisfies p
                    , shrunk    = \b -> toB <$> shrunk p (toA b) }

-- Test if a `t` satisfies a predicate for `a`s when `a :< t`
test :: a :< t => Predicate a -> t -> Bool
test p t = maybe False id (apply p <$> prj t)

-- The predicate which accepts anything
anything :: Arbitrary a => Predicate a
anything = Predicate { apply     = const True
                     , satisfies = arbitrary
                     , shrunk    = elements . shrink 
                     , name      = "anything" }

-- Accepts nothing
nothing :: Predicate a
nothing = Predicate { apply     = const False
                    , satisfies = satisfies nothing
                    , shrunk    = shrunk nothing
                    , name      = "nothing" }

-- Accepts any `Int`
anyInt :: Predicate Int
anyInt = anything { name = "anyInt" }

-- Accepts any positive int
posInt :: Predicate Int
posInt = Predicate { apply     = (>0)
                   , satisfies = fmap ((+1) . abs) arbitrary
                   , shrunk    = \i -> elements (((+1) . abs) <$> shrink i)
                   , name      = "posInt" }

-- Accepts any negative int
negInt :: Predicate Int
negInt = Predicate { apply     = (<0)
                   , satisfies = fmap (negate . (+1) . abs) arbitrary
                   , shrunk    = \i -> elements ((negate . (+1) . abs) <$> shrink i)
                   , name      = "negInt" }

-- Accepts any non-negative int
nonNegInt :: Predicate Int
nonNegInt = Predicate { apply     = (>=0)
                      , satisfies = fmap abs arbitrary
                      , shrunk    = elements . fmap abs . shrink
                      , name      = "nonNegInt" }

-- Accepts anything in the range [p, q]
inRange :: (Ord a, Show a, Arbitrary a) => a -> a ->  Predicate a
inRange p q = Predicate { apply     = \a -> p <= a && a <= q
                        , satisfies = arbitrary `suchThat` (apply (inRange p q))
                        , shrunk    = elements . filter (apply $ inRange p q) . shrink
                        , name      = "inRange " ++ show p ++ " " ++ show q }

-- Accepts any `Double`
anyDouble :: Predicate Int
anyDouble = anything { name = "anyDouble" }

-- Accepts any `Dobule`
anyBool :: Predicate Int
anyBool = anything { name = "anyBool" }

-- Accepts any member of `as`
choiceOf :: (Eq a, Show a) => [a] -> Predicate a
choiceOf as = Predicate { apply     = flip elem as
                        , satisfies = elements as 
                        , shrunk    = \a -> elements (a : takeWhile (/=a) as)
                        , name      = "choiceOf " ++ show as }

-- Accepts any permuation of `as`
permutationOf :: (Eq a, Ord a, Show a) => [a] -> Predicate [a]
permutationOf as = Predicate { apply     = \as' -> sort as == sort as'
                             , satisfies = shuffle as
                             , shrunk    = \as -> shuffle as
                             , name      = "permutationOf " ++ show as }

-- Accepts any superset of `as`
supersetOf :: (Arbitrary a, Eq a, Show a) => [a] -> Predicate [a]
supersetOf as = Predicate { apply     = \as' -> all (`elem` as') as
                          , satisfies = arbitrary >>= \as' -> shuffle (as ++ as')
                          , shrunk    = error $ "Shrinking of \"supersetOf " ++ show as ++ " \" not implemented" 
                          , name      = "supersetOf " ++ show as }

-- Accepts precisely `a`
is :: (Eq a, Show a) => a -> Predicate a
is a = Predicate { apply     = (a==)
                 , satisfies = return a
                 , shrunk    = \a -> return a
                 , name      = "is " ++ show a }

are :: (Eq a, Show a) => [a] -> Predicate [a]
are a = (is a) { name = "are " ++ show a }

-- Accepts any predicate in the list
anyOf :: [Predicate a] -> Predicate a
anyOf ps = Predicate { apply     = (\a -> any (flip apply a) ps)
                     , satisfies = oneof (satisfies <$> ps)
                     , shrunk    = \a -> oneof (flip shrunk a <$> ps)
                     , name      = "anyOf [" ++ intercalate "," (map name ps) ++ "]" }

alphaNumString :: Predicate String
alphaNumString = Predicate { apply     = all isAlphaNum
                           , satisfies = listOf . elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
                           , shrunk    = elements . shrink
                           , name      = "alphaNumString" }

data Unfailing t a = Failed t
                   | OkValue a
                   deriving (Ord, Eq, Show)

instance a :< t => Unfailing t a :< t where
  inj (Failed t)  = t
  inj (OkValue a) = inj a

  prj t = Just $ maybe (Failed t) OkValue (prj t)

unfailing :: Arbitrary t => Predicate a -> Predicate (Unfailing t a)
unfailing p = Predicate { apply     = \uf -> case uf of
                                              Failed _  -> True
                                              OkValue a -> apply p a
                        , satisfies = oneof [ OkValue <$> satisfies p, Failed <$> arbitrary ]
                        , shrunk    = error "Not yet implemented"
                        , name      = "unfailing " ++ name p }
