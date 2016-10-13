{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Control.Monad
import Control.Applicative
import Database.PostgreSQL.Simple.URL
import Control.Monad.IO.Class

import System.Environment
import Data.Text.Lazy.Encoding (decodeUtf8)


data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id


                     

data Menu = Menu { idMenu :: Maybe Int, name :: Maybe String, description :: Maybe String, price :: Maybe Int, restaurant :: Maybe Int } deriving (Show,Generic)
instance ToJSON Menu
instance FromJSON Menu

instance FromRow Menu where
  fromRow = Menu <$> field <*> field <*> field <*> field <*> field
  
  
instance ToRow Menu where
  toRow d = [toField (idMenu d), toField (name d), toField (description d), toField (price d), toField (restaurant d)]


allMenus :: Connection -> IO [Menu]
allMenus c = do
  list <- (query_ c "select * from menu" :: IO [Menu])
  return list

main :: IO ()
main = do
    conn <- connectPostgreSQL  "postgres://bdvyfpprlpcziu:ncmVS1afX7siDrv1aybdcEEuwH@ec2-54-163-251-104.compute-1.amazonaws.com:5432/d91771v4pqfihj"
    env <- getEnvironment
    let port = maybe 8080 read $ lookup "PORT" env
    scotty port $ do
    
     get "/" $ text "Hello, world!"
     
     get "/menus" $ do
      variable <- liftIO (allMenus conn)
      json variable
      
     post "/menus" $ do
      user <- (jsonData :: ActionM Menu)
      response <- liftIO (execute conn "insert into menu (id,name,description,price,restaurant) values (?,?,?,?,?)" $ Menu ((idMenu user)) ( (name user)) ( (description user)) ( (price user)) ((restaurant user)))
      json (response)
      
      




