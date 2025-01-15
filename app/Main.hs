module Main (main) where

import           Data.Text                        (Text)
import           Data.Either                      (isRight)

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)
import Debug.Trace (trace)

import Util

type Model = ()

newtype Action = Echo Text

slowChessBot :: BotApp Model Action
slowChessBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update a | trace (show "updateToAction " ++ showMaybe (updateMessageText update) ++ " -- " ++ show a) False = undefined
updateToAction update _ = case updateMessageText update of
      Just text -> do
        let move = text2Move text
        if isRight move
          then 
          Just (Echo text)
        else
          Nothing
      Nothing   -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Echo msg -> model <# do
    trace (show "handleAction Echo " ++ show msg ++ " -- " ++ show model)
    -- Filter messages and get only those that are a chess move
      pure msg

main :: IO ()
main = do
  token <- getEnvToken "TELEGRAM_BOT_TOKEN"
  env <- defaultTelegramClientEnv token
  startBot_ slowChessBot env
