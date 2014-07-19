{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude hiding (putStrLn)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Format as F
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy.IO (putStrLn)
import           Network.HTTP.Types (status200, status400)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq (responseBody, post)
import           System.Directory (doesFileExist)
import           System.Environment (getArgs)

newtype Channel = Channel Text deriving (Eq, Ord, Show)
newtype User = User Text deriving (Eq, Ord, Show)
newtype Emoji = Emoji Text
data Want = WantsUser User | WantsChannel deriving (Eq, Show, Ord)
data Person = Person User Channel Want deriving (Eq, Show, Ord)
data Notice = Notice Channel Text Emoji

type DB = Map Channel [Person]

instance ToJSON Notice where
  toJSON (Notice (Channel channel) text (Emoji emoji)) =
    object [ "channel" .= ("#" <> channel)
           , "icon_emoji" .= (":" <> emoji <> ":")
           , "parse" .= ("full" :: Text)
           , "username" .= ("hi5bot" :: Text)
           , "text" .= text
           ]

dbPut :: MVar DB -> Person -> IO ()
dbPut dbM person@(Person _ channel _) = MVar.modifyMVar_ dbM (return . f)
  where f db = M.insert channel (person:maybe [] id (M.lookup channel db)) db

dbDelete :: MVar DB -> Person -> IO ()
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
    textOf = decodeUtf8
    params = M.fromList [(textOf k, textOf <$> v) | (k, v) <- queryString raw]
    p key = case M.lookup key params of
      Just (Just "") -> Left ("Empty key in params: " <> key)
      Just (Just value) -> Right value
      _  -> Left ("Unable to find key in params: " <> key)
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
        text = TL.toStrict $ F.format "@{} {}" (giver, addendum)

noticeOfFirstHand :: Person -> Notice
noticeOfFirstHand (Person (User user) channel want) = Notice channel text (Emoji "hand")
  where addendum = case want of
          WantsUser (User user') -> F.format " for @{} to slap." (F.Only user')
          WantsChannel -> "."
        text = TL.toStrict $ F.format "@{} raises a hand high into the air{}" (user, addendum)

postPayload :: String -> Notice -> IO ()
postPayload token notice = do
  args <- getArgs
  case args == ["-n"] of
    True -> do
      putStrLn $ F.format "+ Pretending (-n) to post {}" [(TLE.decodeUtf8 . encode) notice]
    False -> do
      r <- post url (encode notice)
      putStrLn $ TLE.decodeUtf8 (r ^. responseBody)
    where
      url = "https://trello.slack.com/services/hooks/incoming-webhook?token=" <> token

application :: MVar DB -> Application
application dbM rawRequest respond = do
  putStrLn $ F.format "+ Incoming request: {} {}" (map decodeUtf8 [rawPathInfo rawRequest, rawQueryString rawRequest])
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
    respondWithError = respond . responseLBS status400 headers . TLE.encodeUtf8 . TL.fromStrict

main :: IO ()
main = do
  tokenExists <- doesFileExist "token"
  case tokenExists of
    True -> do
      db <- MVar.newMVar M.empty
      putStrLn "+ Listening on port 81"
      run 81 (application db)
    False -> error "Cannot find file containing the token for the incoming webhook"
