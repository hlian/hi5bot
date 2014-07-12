{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Aeson
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (putStrLn)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Format as F
import Network.Wai
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import Prelude hiding (putStrLn)
import System.Directory (doesFileExist)
import System.Environment (getArgs)

newtype Channel = Channel Text deriving (Eq, Ord, Show)
newtype User = User Text deriving (Eq, Ord)
newtype Emoji = Emoji Text
data Want = WantsUser User | WantsChannel deriving (Eq, Show, Ord)
data Person = Person User Channel Want deriving (Eq, Show, Ord)
data Notice = Notice Channel Text Emoji

type DB = Map Channel [Person]

instance Show User where
  show (User s) = T.unpack s

instance ToJSON Notice where
  toJSON (Notice (Channel channel) text (Emoji emoji)) =
    object [ "channel" .= T.concat ["#", channel]
           , "icon_emoji" .= T.concat [":", emoji, ":"]
           , "parse" .= String "full"
           , "username" .= String "hi5bot"
           , "text" .= text
           ]

dbPut :: MVar.MVar DB -> Person -> IO ()
dbPut dbM person@(Person _ channel _) = MVar.modifyMVar_ dbM (return . f)
  where f db = M.insert channel (person:maybe [] id (M.lookup channel db)) db

dbDelete :: MVar.MVar DB -> Person -> IO ()
dbDelete dbM person@(Person _ channel _) = MVar.modifyMVar_ dbM (return . f)
  where f db = M.insert channel [p | p <- maybe [] id (M.lookup channel db), p /= person] db

findPerson :: DB -> Channel -> Want -> Maybe Person
findPerson db channel want = do
  people <- M.lookup channel db
  let predicate user' = case want of
        WantsUser user -> user' == user
        WantsChannel -> True
  listToMaybe [p | p@(Person user' _ _ ) <- people, predicate user']

personOfRequest :: Request -> Either Text Person
personOfRequest raw = do
  channel <- channelOf <$> case p "channel_name" of
    Right "directmessage" -> Left ("Can't high-five in a direct message (yet)!")
    Right "privategroup" -> Left ("Can't high-five in a private group (yet)!")
    x -> x
  user <- userOf <$> p "user_name"

  return $ case text of
    Nothing -> Person user channel WantsChannel
    Just userName -> Person user channel (WantsUser $ userOf userName)

  where
    textOf = decodeUtf8 . L.fromChunks . return
    params = M.fromList [(textOf k, textOf <$> v) | (k, v) <- queryString raw]
    p key = case M.lookup key params of
      Just (Just "") -> Left (T.concat ["Empty key in params: ", key])
      Just (Just value) -> Right value
      _  -> Left (T.concat ["Unable to find key in params: ", key])
    text = case p "text" of Left _ -> Nothing; Right x -> Just x
    userOf = User . T.filter (/= '@')
    channelOf = Channel

-- Giver -> Receiver
noticeOfSecondHand :: Person -> Person -> Notice
noticeOfSecondHand (Person (User giver) _ _) (Person (User receiver) channel want) = Notice channel text (Emoji "rainbow")
  where addendum = case giver == receiver of
          True -> "touches a hand to the other hand while people avert their eyes."
          False -> case want of
            WantsUser (User user')
              | giver /= user' -> F.format "swoops in for a high-five with @{}! (Caw caw! Better luck next time, @{}.) :hand::octopus:" (receiver, user')
              | otherwise -> F.format "high-fives @{}, and everybody's eyes widen with respect. :hand::guitar:" (F.Only receiver)
            WantsChannel -> F.format "high-fives @{}! :hand:" (F.Only receiver)
        text = F.format "@{} {}" (giver, addendum)

noticeOfFirstHand :: Person -> Notice
noticeOfFirstHand (Person (User user) channel want) = Notice channel text (Emoji "hand")
  where addendum = case want of
          WantsUser (User user') -> F.format " for @{} to slap." (F.Only user')
          WantsChannel -> "."
        text = F.format "@{} raises a hand high into the air{}" (user, addendum)

postPayload :: String -> Notice -> IO ()
postPayload token notice = do
  args <- getArgs
  case args == ["-n"] of
    True -> do
      putStrLn (T.concat ["+ Pretending (-n) to post ", (decodeUtf8 . encode) notice])
    False -> do
      response <- get request
      putStrLn (decodeUtf8 (H.responseBody response))
    where
      baseRequest = H.parseUrl ("https://trello.slack.com/services/hooks/incoming-webhook?token=" ++ token)
      request = fmap (H.urlEncodedBody [("payload", bytes)]) baseRequest
      get r = r >>= (H.withManager . H.httpLbs)
      bytes = (B.concat . L.toChunks . encode) notice

application :: MVar.MVar DB -> Application
application dbM rawRequest respond = do
  putStrLn $ T.concat ["+ Incoming request: "
                      , decodeUtf8 $ L.fromChunks [rawPathInfo rawRequest]
                      , decodeUtf8 $ L.fromChunks [rawQueryString rawRequest]]
  db <- MVar.readMVar dbM
  case personOfRequest rawRequest of
    Left err -> respondWithError err
    Right person@(Person _ channel want) -> case findPerson db channel want of
      Just receiver -> (dbDelete dbM receiver >> postGive person receiver >> respondWithEmpty)
      Nothing -> (dbPut dbM person >> postWant person >> respondWithEmpty)
  where
    headers = [("Content-Type", "text/plain")]
    tokenM = fmap (filter (/= '\n')) (readFile "token")
    postWant receiver = tokenM >>= \t -> postPayload t (noticeOfFirstHand receiver)
    postGive giver receiver = tokenM >>= \t -> postPayload t (noticeOfSecondHand giver receiver)
    respondWithEmpty = (respond . responseLBS status200 headers) ""
    respondWithError = respond . responseLBS status400 headers . encodeUtf8

main :: IO ()
main = do
  tokenExists <- doesFileExist "token"
  case tokenExists of
    True -> do
      db <- MVar.newMVar M.empty
      putStrLn "+ Listening on port 81"
      run 81 (application db)
    False -> error "Cannot find file containing the token for the incoming webhook"
