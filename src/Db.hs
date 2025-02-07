module Db (dbGet, dbSave, dbDelete) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.Redis (Connection, runRedis, del, get, set)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeLatin1)

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
