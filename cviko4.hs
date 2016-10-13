module Cviko4 where

import Prelude

-- class - typová třída s operacemi
-- instance - definice operací typové třídy pro konkrétní typ

-- instance Eq Bool where
--  True == True = True
--  False == False = True
--  _ == _ = False

-- dědění
-- class Eq a => Ord a where ...

-- show = toString

class Visible a where
  toString :: a -> String
  
instance Visible Char where
  toString ch = [ch]
  
instance Visible a => Visible [a] where
  toString = concat . map toString

--instance (Visible a) => Show a where
--  show b = toString b
  
instance (Visible a, Visible b) => Visible (a, b) where
  toString (x, y) = "(" ++ (toString x) ++ ", " ++ (toString y) ++ ")"