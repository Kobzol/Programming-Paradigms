module Cviko6 where

import Prelude

-- newtype Queue a = Qu [a]
-- vznikne nový typ, který nededí typové trídy

-- return a -> Monad a
-- return obalí hodnotu monádou

-- sequence :: [IO()] -> IO ()

ready :: IO Bool
ready = do
      c <- getChar
      return (c == 'y')
      
main = do
  c <- getChar
  putChar c