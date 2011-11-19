module RedBlackSet(RedBlackSet) where

data Color = R | B deriving Show
data RedBlackSet a = E | T Color (RedBlackSet a) a (RedBlackSet a) deriving Show
  
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b

empty :: RedBlackSet a
empty = E
  
member :: Ord a => a -> RedBlackSet a -> Bool
member x E = False
member x (T _ a y b) =
  if x < y then member x a
  else if x > y then member x b
       else True
                              
insert :: Ord a => RedBlackSet a -> a -> RedBlackSet a
insert s x = T B a y b
  where ins E = T R E x E
        ins s@(T color a y b) =
          if x < y then balance color (ins a) y b
          else if x > y then balance color a y (ins b)
                             else s
        T _ a y b = ins s


fromOrdList :: Ord a => [a] -> RedBlackSet a
fromOrdList xs = foldl' insert empty xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = let z' = f z x
                        in seq z' $ foldl' f z' xs