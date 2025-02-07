{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Text                        (Text, pack, unpack)
import Data.Maybe                                 (isJust, fromJust)
import Control.Monad.IO.Class
import Data.Text.Encoding
import Debug.Trace                                (trace, traceM)

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

import Game.Chess
import Game.Chess.SAN

import Database.Redis

import Util


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
updateToAction update _ | trace ("updateToAction " ++ showMaybe (updateMessageText update)) False = undefined
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

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Answer msg -> model <# do
    pure msg
  AttemptMove moveText chatID -> model <# do
    let conn = redisConnection model
    let chatIDBS = encodeUtf8 $ pack $ show $ chatID
    boardDB <- liftIO $ runRedis conn $ get chatIDBS
    _ <- traceM ("boardDB " ++ show boardDB)
    case boardDB of
      Right (Just boardFEN) -> do
        case fromFEN $ unpack $ decodeLatin1 boardFEN of
          Just board -> do
            _ <- traceM ("board " ++ show board)
            case fromSAN board moveText of
              Right ply -> do
                let newPosition = unsafeDoPly board ply
                let newBoard = encodeUtf8 $ pack $ toFEN newPosition
                _ <- liftIO $ runRedis conn $ set chatIDBS newBoard
                pure $ decodeLatin1 newBoard
              Left _ -> pure "Illegal move from board"
          Nothing -> pure "Coulnd't parse board from FEN"
      Right (Nothing) -> do
        _ <- traceM ("new board")
        case fromSAN startpos moveText of
          Right ply -> do
            let newPosition = unsafeDoPly startpos ply
            let newBoard = encodeUtf8 $ pack $ toFEN newPosition
            _ <- liftIO $ runRedis conn $ set chatIDBS newBoard
            pure $ decodeLatin1 newBoard
          Left _ -> pure "Illegal initial move"
      Left _ -> pure "Something went wrong when trying to connect to the DB"

main :: IO ()
main = do
  redis <- connect defaultConnectInfo
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  env <- defaultTelegramClientEnv token
  startBot_ (slowChessBot redis) env
