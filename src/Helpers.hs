{-# LANGUAGE TypeFamilies #-}

module Helpers
  (until
  ,untilEol
  ,numbers)
  where

import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              as T
import           Data.Time
import           Data.Time.Format
import           Prelude                hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.String

until :: String -> Parsec Dec String String
until s = anyChar `someTill` string s

untilEol :: Parsec Dec String String
untilEol = anyChar `someTill` newline

numbers :: Parsec Dec String String
numbers = some numberChar
