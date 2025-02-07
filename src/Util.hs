{-# LANGUAGE OverloadedStrings #-}

module Util (
  showMaybe,
  getMove,
) where

import Data.Maybe (isJust)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Data.Char as Char ( isAscii )

showMaybe :: Show a => Maybe a -> [Char]
showMaybe m
  | isJust m = show m
  | otherwise = ""

trim :: Text -> Text
trim = Text.filter Char.isAscii

-- Use an indicator "->" to identify a chess move
getMove :: Text -> Maybe Text
getMove text = case Text.words (trim text) of
  [] -> Nothing
  (_:[]) -> Nothing
  (indicator:moveText:_) -> 
    if indicator /= "#" then Nothing else
    if (Text.length moveText) > 8 then Nothing else Just moveText
  