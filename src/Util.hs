{-# LANGUAGE OverloadedStrings #-}

module Util (
  showMaybe,
  getMove,
) where

import Data.Maybe (isJust)
import           Data.Text                        (Text, pack, unpack)
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
getMove text = case unpack (trim text) of
  [] -> Nothing
  (_:[]) -> Nothing
  (indicator:moveText) -> 
    if indicator /= '/' then Nothing else do
      let moveChars = length moveText 
      if (moveChars > 8 || moveChars < 2) then Nothing else Just (pack moveText)
  