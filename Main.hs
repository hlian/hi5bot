{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Error (throwError)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Network.Wai
import qualified Network.HTTP.Conduit as H
import Network.HTTP.Types (status200, status400)
import Network.Wai.Handler.Warp (run)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.JSON
import Text.Printf

type Channel = U.ByteString
newtype User = User String deriving (Eq, Ord)
data Want = WantsUser User | WantsChannel deriving (Eq, Show, Ord)
data Person = Person User Channel Want deriving (Eq, Show, Ord)

type DB = Map Channel [Person]

instance Show User where
  show (User s) = s

userOf :: U.ByteString -> User
userOf = User . filter (/= '@') . U.toString

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

personOfRequest :: Request -> Either String Person
personOfRequest raw = do
  channel <- case p "channel_name" of
    Right "directmessage" -> throwError ("Can't high-five in a direct message!")
    Right "privategroup" -> throwError ("Can't high-five in a private group!")
    x -> x
  user <- p "user_name"

  return $ case text of
    Nothing -> Person (userOf user) channel WantsChannel
    Just userName -> Person (userOf user) channel (WantsUser $ userOf userName)

  where
    params = M.fromList (queryString raw)
    p key = case M.lookup key params of
      Just (Just "") -> throwError ("Empty key in params: " ++ show key)
      Just (Just value) -> return value
      _  -> throwError ("Unable to find key in params: " ++ show key)
    text = case p "text" of Left _ -> Nothing; Right x -> Just x

-- Giver -> Receiver
jsonOfSecondHand :: Person -> Person -> JSObject U.ByteString
jsonOfSecondHand (Person giver _ _) (Person receiver channel want) = toJSObject pairs
  where pairs = [ ("channel", B.concat ["#", channel])
                , ("username", "hi5bot")
                , ("text", U.fromString $ printf "@%s %s" (show giver) addendum)
                , ("icon_emoji", ":rainbow:")
                , ("parse", "full")
                ]
        addendum :: String
        addendum = case giver == receiver of
          True -> "touches a hand to the other hand while people avert their eyes."
          False -> case want of
            WantsUser user' | giver /= user' -> printf "swoops in for a high-five with @%s! (Better luck next time, @%s.) :hand::octopus:" (show receiver) (show user')
                            | otherwise -> printf "completes the high-five with @%s, and it's awesome. :hand::guitar:" (show receiver)
            WantsChannel -> printf "high-fives @%s! :hand:" (show receiver)

jsonOfFirstHand :: Person -> JSObject U.ByteString
jsonOfFirstHand (Person user channel want) = toJSObject pairs
  where pairs = [ ("channel", B.concat ["#", channel])
                , ("username", "hi5bot")
                , ("text", U.fromString $ printf "@%s formally requests a %s." (show user) addendum)
                , ("icon_emoji", ":hand:")
                , ("parse", "full")
                ]
        addendum :: String
        addendum = case want of
          WantsUser user' -> printf "`/hi5 %s` from @%s" (show user) (show user')
          WantsChannel -> "`/hi5`"

postPayload :: (JSON a) => String -> a -> IO ()
postPayload token payload = do
  args <- getArgs
  case args == ["-n"] of
    True -> do
      putStrLn ("+ Pretending (-n) to post " ++ (encode payload))
      return ()
    False -> do
      response <- get request
      putStrLn (show $ H.responseBody response)
      return ()
    where
      baseRequest = H.parseUrl ("https://trello.slack.com/services/hooks/incoming-webhook?token=" ++ token)
      request = fmap (H.urlEncodedBody pairs) baseRequest
      get r = r >>= (H.withManager . H.httpLbs)
      pairs = [("payload", (U.fromString . encode) payload)]

application :: MVar.MVar DB -> Application
application dbM rawRequest respond = do
  putStrLn ("+ Incoming request: " ++ (show $ rawPathInfo rawRequest) ++ (show $ rawQueryString rawRequest))
  db <- MVar.readMVar dbM
  case personOfRequest rawRequest of
    Left err -> respondWithError err
    Right person@(Person _ channel want) -> case findPerson db channel want of
      Just receiver -> (dbDelete dbM receiver >> postGive person receiver >> respondWithEmpty)
      Nothing -> (dbPut dbM person >> postWant person >> respondWithEmpty)
  where
    headers = [("Content-Type", "text/plain")]
    tokenM = fmap (filter (/= '\n')) (readFile "token")
    postWant receiver = tokenM >>= \t -> postPayload t (jsonOfFirstHand receiver)
    postGive giver receiver = tokenM >>= \t -> postPayload t (jsonOfSecondHand giver receiver)
    respondWithEmpty = (respond . responseLBS status200 headers) ""
    respondWithError = respond . responseLBS status400 headers . L.fromStrict . U.fromString

main :: IO ()
main = do
  tokenExists <- doesFileExist "token"
  case tokenExists of
    True -> do
      db <- MVar.newMVar M.empty
      putStrLn "+ Listening on port 81"
      run 81 (application db)
    False -> error "Cannot find file containing the token for the incoming webhook"
