{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

data User =
  User { userId :: Integer
       , username :: Text
       , shell :: Text
       , homeDirectory :: Text
       , realName :: Text
       , phone :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User {..}) = toRow
    (userId, username, shell, homeDirectory, realName, phone)

createUserTable :: Query
createUserTable = [r|
create table if not exists users (
  id integer primary key autoincrement,
  username text unique,
  shell text,
  home_directory text,
  real_name text,
  phone text
)
|]

insertUser :: Query
insertUser = [r|
insert into users (username, shell, home_directory, real_name, phone)
values (?, ?, ?, ?, ?)
|]

getUser :: Query
getUser = [r|
select * from users where username = ?
|]

allUsers :: Query
allUsers = [r|
select * from users
|]

data DuplicateUser = DuplicateUser deriving (Eq, Show, Typeable)
instance Exception DuplicateUser

type UserRow = (Text, Text, Text, Text, Text)

findUser :: Connection -> Text -> IO (Maybe User)
findUser conn name = do
  results <- query conn getUser (Only name)
  case results of
    [] -> return Nothing
    [u] -> return $ Just u
    _ -> throwIO DuplicateUser

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUserTable
  execute conn insertUser derrick
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where derrick :: UserRow
        derrick = ("Derrick", "cmd.exe", "C:/Users/Derrick",
                   "Derrick W. Turk", "936-520-7752")

returnUsers :: Connection -> Socket -> IO ()
returnUsers conn sock = do
  rows <- query_ conn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll sock $ encodeUtf8 newlineSeparated

formatUser :: User -> ByteString
formatUser (User {..}) = BS.concat
  [ "Login: ", encodeUtf8 username, "\t\t\t\t"
  , "Name: ", encodeUtf8 realName, "\n"
  , "Directory: ", encodeUtf8 homeDirectory, "\t\t\t"
  , "Shell: ", encodeUtf8 shell, "\n"
  ]

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser conn sock username = do
  user <- findUser conn $ T.strip username
  case user of
    Nothing -> do
      putStrLn $ "Couldn't find " ++ show username
      return ()
    Just user -> sendAll sock $ formatUser user

handleQuery :: Connection -> Socket -> IO ()
handleQuery conn sock = do
  msg <- recv sock 1024
  case msg of
    "\r\n" -> returnUsers conn sock
    name -> returnUser conn sock (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries conn sock = forever $ do
  (sock', _) <- accept sock
  putStrLn "accepted inbound connection"
  handleQuery conn sock'
  close sock'

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
    (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
    Nothing
    (Just "79")
  let serverAddr = head addrinfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
