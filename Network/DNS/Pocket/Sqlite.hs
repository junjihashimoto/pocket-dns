{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DNS.Pocket.Sqlite (
  DnsBackend
) where
import Data.IP
import Network.DNS hiding (lookup)
import Network.Socket hiding (recvFrom)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import qualified Data.Yaml as Y
import Network.DNS.Pocket.Type

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DnsRecord
    name Domain
    addr [IP]
    DnsRecordU name
    deriving Show
    deriving Eq
|]

instance DnsBackend SqliteConf where
  type Conn SqliteConf = ConnectionPool
  load file = do
    mconf <- Y.decodeFile file
    case mconf of
      Just val -> do 
        return $ Y.parseMonad Database.Persist.loadConfig val
      Nothing -> return Nothing
  setup p = do
    withSocketsDo $ do
      conn <- createPoolConfig p
      return $ Just conn
  getRecord p domain conn = do
    mRecord <- runPool p (getBy (DnsRecordU domain)) conn
    case mRecord of
      Just (Entity _key val') -> do
        let val = dnsRecordAddr val'
        return val
      Nothing ->
        return []
  setRecord p domain ips conn = do
    a <- runPool p (insertUnique (DnsRecord domain ips)) conn
    case a of
      Just _ -> return True
      Nothing -> return False
  listRecord p conn = do
    a <- runPool p (selectList [] []) conn
    return $ map (\(Entity _key val) -> (dnsRecordName val,dnsRecordAddr val)) a
  deleteRecord p q conn = do
    runPool p (deleteBy (DnsRecordU q)) conn
