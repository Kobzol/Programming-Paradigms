module Cviko6 where

import Prelude

-- newtype Queue a = Qu [a]
-- vznikne nov� typ, kter� neded� typov� tr�dy

-- return a -> Monad a
-- return obal� hodnotu mon�dou

-- sequence :: [IO()] -> IO ()

ready :: IO Bool
ready = do
      c <- getChar
      return (c == 'y')
      
main = do
  c <- getChar
  putChar c