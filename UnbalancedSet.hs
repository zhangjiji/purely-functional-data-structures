{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module UnbalancedSet (UnbalancedSet) where
import Set

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)
                     deriving(Show)
                                   
instance Ord a => Set UnbalancedSet a where
  empty = E
  
  insert e E = T E e E
  insert e s@(T a y b) = if e < y then T (insert e a) y b
                         else if e > y then T a y (insert e b)
                              else s
    
  member e E = False
  member e (T a y b) = if e < y then member e a
                       else if e > y then member e b
                            else True
  
