module Util (showMaybe, isMove, text2Move) where

import Data.Maybe (isJust)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import Data.Char as Char ( isAscii )
import Game.Chess
import Game.Chess.SAN

showMaybe :: Show a => Maybe a -> [Char]
showMaybe m
  | isJust m = show m
  | otherwise = ""

trim :: Text -> Text
trim = Text.filter Char.isAscii

isMove :: Text -> Bool
isMove text 
  | length (Text.words text) > 1 = False
  | Text.length (trim text) > 8 = False
  | otherwise = True

text2Move :: Text -> Either String Ply
-- TODO: Add custom position
text2Move text
  | isMove text = fromSAN startpos text
  | otherwise = Left "Wrong move"
  