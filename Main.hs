{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Error (throwError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
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

messageOfRequest :: DB -> Request -> Either String Message
messageOfRequest db raw = do
  channelName <- case p "channel_name" of
    Right "directmessage" -> throwError ("Can't high-five in a direct message!")
    x -> x
  userName <- p "user_name"
  let reply = M.lookup channelName db
  return Message { messageChannelName = channelName
                 , messageUserName = userName
                 , messageReply = reply }
  where
    params = M.fromList (queryString raw)
    p key = case M.lookup key params of
      Just (Just value) -> return value
      _  -> throwError ("Unable to find key in params: " ++ show key)

jsonOfReply :: Message -> Message -> JSObject U.ByteString
jsonOfReply msg reply = toJSObject pairs
  where pairs = [ ("channel", B.concat ["#", messageChannelName msg])
                , ("username", "hi5bot")
                , ("text", U.fromString $ printf "@%s just high-fived @%s!" (U.toString $ messageUserName msg) (U.toString $ messageUserName reply))
                , ("icon_emoji", ":rainbow:")
                ]

jsonOfOffer :: Message -> JSObject U.ByteString
jsonOfOffer msg = toJSObject pairs
  where pairs = [ ("channel", B.concat ["#", messageChannelName msg])
                , ("username", "hi5bot")
                , ("text", U.fromString $ printf "@%s raises a hand..." (U.toString $ messageUserName msg))
                , ("icon_emoji", ":hand:")
                ]

postPayload :: (JSON a) => a -> IO ()
postPayload payload = do
  request0 <- H.parseUrl "https://trello.slack.com/services/hooks/incoming-webhook?token=XXX"
  let request = H.urlEncodedBody pairs request0
  response <- H.withManager (H.httpLbs request)
  putStrLn (show $ H.responseBody response)
  return ()
    where pairs = [("payload", (U.fromString . encode) payload)]

application :: MVar.MVar DB -> Application
application dbM rawRequest respond = do
  putStrLn ("Incoming request: " ++ (show $ rawPathInfo rawRequest) ++ (show $ rawQueryString rawRequest))
  db <- MVar.readMVar dbM
  case messageOfRequest db rawRequest of
    Left err -> respondWithError err
    Right msg -> case messageReply msg of
      Just reply -> (dbDelete dbM msg >> postReply msg reply >> respondWithEmpty)
      Nothing -> (dbPut dbM msg >> postOffer msg >> respondWithEmpty)
  where
    headers = [("Content-Type", "text/plain")]
    postOffer msg = postPayload $ jsonOfOffer msg
    postReply msg reply = postPayload $ jsonOfReply msg reply
    respondWithEmpty = (respond . responseLBS status200 headers) ""
    respondWithError = respond . responseLBS status400 headers . L.fromStrict . U.fromString

main = do
  db <- MVar.newMVar M.empty
  run 81 (application db)

