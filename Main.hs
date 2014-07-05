{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Network.Wai
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Text.JSON
import Text.Printf

data Message = Message { messageChannelName :: U.ByteString
                       , messageUserName :: U.ByteString
                       , messageReply :: Maybe Message
                       } deriving (Show)

type DB = M.Map U.ByteString Message

dbPut :: MVar.MVar DB -> Message -> IO ()
dbPut dbM message = MVar.modifyMVar_ dbM (\db -> return $ M.insert (messageChannelName message) message db)

dbDelete :: MVar.MVar DB -> Message -> IO ()
dbDelete dbM message = MVar.modifyMVar_ dbM (\db -> return $ M.delete (messageChannelName message) db)

messageOfRequest :: DB -> Request -> Maybe Message
messageOfRequest db raw = do
  channelName <- p "channel_name"
  userName <- p "user_name"
  let reply = M.lookup channelName db
  return Message { messageChannelName = channelName
                 , messageUserName = userName
                 , messageReply = reply }
  where
    params = M.fromAscList (queryString raw)
    p key = case M.lookup key params of
      Just (Just value) -> Just value
      _  -> Nothing

jsonOfReply :: Message -> Message -> JSObject U.ByteString
jsonOfReply msg reply = toJSObject pairs
  where pairs = [ ("channel", B.concat ["#", messageChannelName msg])
                , ("username", "hi5bot")
                , ("text", U.fromString $ printf "@%s just high-fived @%s!" (U.toString $ messageUserName msg) (U.toString $ messageUserName reply))
                , ("icon_emoji", ":hand:")
                ]

postReply :: Message -> Message -> IO ()
postReply msg reply = do
  request0 <- H.parseUrl "https://trello.slack.com/services/hooks/incoming-webhook?token=XXX"
  let request = H.urlEncodedBody pairs request0
  response <- H.withManager (H.httpLbs request)
  putStrLn (show $ H.responseBody response)
  return ()
    where pairs = [("payload", (U.fromString . encode) (jsonOfReply msg reply))]

application :: MVar.MVar DB -> Application
application dbM rawRequest respond = do
  putStrLn ("Incoming request: " ++ (show $ rawPathInfo rawRequest) ++ (show $ rawQueryString rawRequest))
  db <- MVar.readMVar dbM
  case messageOfRequest db rawRequest of
    Just msg -> case messageReply msg of
      Just reply -> (dbDelete dbM msg >> postReply msg reply >> respondWithEmpty)
      Nothing -> (dbPut dbM msg >> respondWithEmpty)
    _ -> respondWithError "The hi5 bot is on the fritz!"
  where
    headers = [("Content-Type", "text/plain")]
    respondWithEmpty = (respond . responseLBS status200 headers) ""
    respondWithError = respond . responseLBS status400 headers

main = do
  db <- MVar.newMVar M.empty
  run 81 (application db)

