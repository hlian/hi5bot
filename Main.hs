{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

import           BasePrelude hiding (readFile)
import           Network.Linklater

import qualified Control.Concurrent.MVar as MVar
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.IO (readFile)
import           Network.Wai.Handler.Warp (run)

data Want = WantsUser Text | WantsChannel deriving (Eq, Show, Ord)
data Person = Person User Channel Want deriving (Eq, Show, Ord)
type DB = Map Channel [Person]

dbGet :: DB -> Channel -> [Person]
dbGet db channel =
  maybe [] id (M.lookup channel db)

dbPut :: DB -> Person -> DB
dbPut db person@(Person _ channel _) =
  M.insert channel (person:dbGet db channel) db

dbDelete :: DB -> Person -> DB
dbDelete db person@(Person _ channel _) =
  M.insert channel [p | p <- dbGet db channel, p /= person] db

findPerson :: DB -> Channel -> Want -> Maybe Person
findPerson db channel want = do
  people <- M.lookup channel db
  let predicate user' = case want of
        WantsUser user -> user' == user
        WantsChannel -> True
  listToMaybe [p | p@(Person (User user') _ _ ) <- people, predicate user']

foundMessage :: Person -> Person -> Message
foundMessage (Person subject _ _) (Person object channel want) =
  SimpleMessage (EmojiIcon "rainbow") "hi5bot" channel ("@" <> u subject <> " " <> body)
  where
    body =
      case subject == object of
       True ->
         "touches a hand to the other hand while people avert their eyes."
       False ->
         case want of
          WantsUser objectDesire
            | subject /= (User objectDesire) ->
                "swoops in for a high-five with @"
                <> u object
                <> ". Caw caw! Better luck next time, @"
                <> objectDesire
                <> ". :hand::octopus:"
            | otherwise ->
                "high-fives @"
                <> u object
                <> ", and everybody's eyes widen with respect. :hand::guitar:"
          WantsChannel ->
            "high-fives @"
            <> u object
            <> "! :hand:"
    u (User x) = x

wantMessage :: Person -> Message
wantMessage (Person user channel want) =
  SimpleMessage (EmojiIcon "hand") "hi5bot" channel ("@" <> u user <> " " <> body)
  where
    body =
      case want of
       WantsUser objectDesire ->
         "raises a hand high in the air for @"
         <> objectDesire
         <> " to `/hi5`. A high-five is afoot!"
       WantsChannel ->
         "raises a hand high in the air."
    u (User x) = x

cheatMode :: Bool
cheatMode = False

parseWant :: Maybe Text -> Want
parseWant t =
  case t' of
   Just text ->
     case TL.length text of
      0 -> WantsChannel
      _ -> WantsUser text
   Nothing -> WantsChannel
  where
    t' = TL.strip <$> t

parseCommand :: Command -> (User, Want)
parseCommand (Command user _ maybeText) =
  case (cheatMode, (map TL.strip . TL.splitOn "--") <$> maybeText) of
   (True, Just [text', user']) ->
     (User user', parseWant (return text'))
   _ ->
     (user, parseWant maybeText)

hi5 :: MVar DB -> Config -> Maybe Command -> IO Text
hi5 dbM config (Just command@(Command _ channel _)) = do
  MVar.modifyMVar_ dbM $ \db -> do
    case findPerson db channel want of
     Just giver -> do
       let db' = dbDelete db giver
       void (say (foundMessage person giver) config)
       return db'
     Nothing -> do
       let db' = dbPut db person
       void (say (wantMessage person) config)
       return db'
  return ""
  where
    (user, want) =
      parseCommand command
    person =
      Person user channel want

hi5 _ _ Nothing = do
  return "hi5bot is a high-five robot. It's a robot that helps you give and get high-fives. You can't high-five hi5bot. (Yet.) <https://github.com/hlian/hi5bot>"

main :: IO ()
main = do
  db <- MVar.newMVar M.empty
  token <- TL.filter (/= '\n') <$> readFile "token"
  putStrLn ("+ Listening on port " <> show port)
  run port (slashSimple (hi5 db (Config "trello.slack.com" token)))
  where
    port = 8000
