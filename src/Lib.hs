-- |
-- Module:      Lib
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark.fine@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Lib module.

module Lib
  ( lib
  ) where

import BasicPrelude

lib :: IO ()
lib = putStrLn "lib"
