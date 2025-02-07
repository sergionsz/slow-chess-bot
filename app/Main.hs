{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text                        (Text, pack, unpack)
import Data.Maybe                                 (isJust, fromJust)
import Control.Monad.IO.Class
import Data.Text.Encoding

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

import Game.Chess
import Game.Chess.SAN

import Database.Redis

import Util
import Board (fenToimage)
import Data.Char (toLower)


data Model = Model { redisConnection :: Connection }

data Action = AttemptMove Text ChatId | Answer Text

slowChessBot :: Connection -> BotApp Model Action
slowChessBot conn = BotApp
  { botInitialModel = Model { redisConnection = conn }
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

getChatId :: Update -> ChatId
getChatId update = case updateMessage update of
  Just message -> chatId $ messageChat message
  Nothing -> ChatId 0

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = case updateMessageText update of
      Just text -> do
        let moveText = getMove text
        let chatID = getChatId update
        if chatID == ChatId 0
          then
            Just (Answer "Can't find chatID")
        else if isJust moveText
          then 
          Just (AttemptMove (fromJust moveText) chatID)
        else Nothing
      Nothing   -> Nothing


dbGet :: MonadIO m => Connection -> Text -> m(Maybe Text)
dbGet redisConn key = do
  let keyBB = encodeUtf8 key
  result <- liftIO $ runRedis redisConn $ get keyBB
  
  case result of
    Right (Just value) -> return $ Just $ decodeLatin1 value
    Right _ -> return Nothing
    -- TODO: Handle errors differently than just returning Nothing
    Left _ -> return Nothing

dbSave :: MonadIO m => Connection -> Text -> Text -> m ()
dbSave redisConn key value = do
  let keyBB = encodeUtf8 key
  let valueBB = encodeUtf8 value
  _ <- liftIO $ runRedis redisConn $ set keyBB valueBB
  return ()

dbDelete :: MonadIO m => Connection -> Text -> m ()
dbDelete redisConn key = do
  let keyBB = encodeUtf8 key
  _ <- liftIO $ runRedis redisConn $ del [keyBB]
  return ()

sendImage :: Integer -> Text -> Maybe Ply -> BotM Text
sendImage chatID fen maybePly = do
  let sanMove = case maybePly of {
    Just ply -> ((map toLower (show $ plySource ply)), (map toLower (show $ plyTarget ply))) ;
    -- TODO: Change Board lib so that the ply is optional
    Nothing -> ("", "")
  }
  let imageName = "board" ++ (show chatID) ++ ".png"
  let inputFile = InputFile imageName "image/png"
  _ <- liftIO $ fenToimage (unpack fen) sanMove 800 imageName
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
