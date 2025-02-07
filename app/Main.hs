{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text                        (Text, pack, unpack, unlines, concat)
import Data.Maybe                                 (isJust, fromJust)
import Control.Monad.IO.Class

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

import Game.Chess
import Game.Chess.SAN

import Database.Redis

import Util
import Db
import Board (fenToimage)
import Data.Char (toLower)


data Model = Model { redisConnection :: Connection }

data Action = AttemptMove Text ChatId | Answer Text

helpText :: Text
helpText = Data.Text.unlines
      [ "Wellcome to Slow Chess!"
      , ""
      , "I can help you play correspondence chess with a friend"
      , ""
      , Data.Text.concat ["To use me, just add me and your friend to a Telegram group "
      , "and start a game by sending the first move in SAN notation"]
      , ""
      , Data.Text.concat ["For example, you can use the very popular /e4 "
      , "or maybe /Nf3 is more your style"]
      , ""
      , Data.Text.concat ["The game will finish on checkmate or stalemate "
      , "after which you can start a new game with a new starting move"]
      ]

slowChessBot :: Connection -> BotApp Model Action
slowChessBot conn = BotApp
  { botInitialModel = Model { redisConnection = conn }
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case updateMessageText update of
      Just text -> do
        let trimmed = trim text
        if trimmed == pack "/help" || trimmed == pack "/start" then Just (Answer helpText) else do
          let moveText = getMove trimmed
          let chatID = getChatId update
          if isJust moveText && chatID /= ChatId 0
            then 
            Just (AttemptMove (fromJust moveText) chatID)
          else Nothing
      Nothing   -> Nothing

sendImage :: Integer -> Text -> Maybe Ply -> BotM Text
sendImage chatID fen maybePly = do
  let sanMove = case maybePly of {
    Just ply -> Just ((map toLower (show $ plySource ply)), (map toLower (show $ plyTarget ply))) ;
    Nothing -> Nothing
  }
  let imageName = "board" ++ (show chatID) ++ ".png"
  let inputFile = InputFile imageName "image/png"
  _ <- liftIO $ fenToimage (unpack fen) sanMove Nothing imageName
  let photoFile = MakePhotoFile inputFile
  let sendPhotoRequest = defSendPhoto (SomeChatId (ChatId chatID)) photoFile
  response <- liftClientM $ sendPhoto sendPhotoRequest
  if not $ responseOk response
  then
    pure "Can't return image"
  else
    pure fen

performMove :: Position -> Text -> Integer -> Connection -> BotM Text
performMove board sanMove chatID conn = case fromSAN board sanMove of
  Right ply -> do
    let chatIDT = pack $ show chatID
    let newPosition = unsafeDoPly board ply
    let newBoard = pack $ toFEN newPosition
    let isGameFinished = length (legalPlies newPosition) == 0
    _ <- if isGameFinished then dbDelete conn chatIDT else dbSave conn chatIDT newBoard
    sendImage chatID newBoard (Just ply)
  Left _ -> pure "Illegal move"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Answer msg -> model <# do
    pure msg
  AttemptMove moveText (ChatId chatID) -> model <# do
    let conn = redisConnection model
    let chatIDT = pack $ show chatID
    boardDB <- dbGet conn chatIDT
    case boardDB of
      Just boardFEN -> do
        case fromFEN $ unpack boardFEN of
          Just board -> do
            performMove board moveText chatID conn
          Nothing -> pure "Coulnd't parse board from FEN"
      Nothing -> do
        performMove startpos moveText chatID conn

main :: IO ()
main = do
  redis <- connect defaultConnectInfo
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  env <- defaultTelegramClientEnv token
  startBot_ (slowChessBot redis) env
