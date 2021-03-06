{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.DNS.Pocket.Type where
import Data.Default
import Data.IP
import Network.DNS hiding (lookup)
import Database.Persist
import Database.Persist.Sqlite (SqliteConf)
import Database.Persist.Zookeeper (ZookeeperConf)

instance PersistField IP where
  toPersistValue (IPv4 ip) = toPersistValue $ fromIPv4 ip
  toPersistValue (IPv6 ip) = toPersistValue $ fromIPv6 ip
  fromPersistValue value = do
    v <- fromPersistValue value
    if length v == 4
      then return $ IPv4 $ toIPv4 $ v
      else return $ IPv6 $ toIPv6 $ v

data DnsConf = DnsConf {
    bufSize :: Int
  , timeOut :: Int
}

instance Default DnsConf where
    def = DnsConf {
        bufSize = 512
      , timeOut = 3 * 1000 * 1000
    }

class DnsBackend p where
  type Conn p
  load :: FilePath -> IO (Maybe p)
  setup :: p -> IO (Maybe (Conn p))
  getRecord :: p -> Domain -> Conn p -> IO [IP]
  setRecord :: p -> Domain -> [IP] -> Conn p -> IO Bool
  deleteRecord :: p -> Domain -> Conn p -> IO ()
  listRecord :: p -> Conn p -> IO [(Domain,[IP])]


data Conf =
    Zookeeper ZookeeperConf
  | Sqlite SqliteConf
