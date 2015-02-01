{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE TypeFamilies, EmptyDataDecls, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}


module Network.DNS.Pocket.Server where
import Control.Applicative
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as S
import Data.ByteString.Lazy hiding (putStrLn, filter, length)
import Data.Default
import Data.IP
import Data.Maybe
import Data.Monoid
import Network.BSD
import Network.DNS hiding (lookup)
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString
import System.Timeout
import System.Environment
import Database.Persist
import Database.Persist.Zookeeper
import Database.Persist.TH
import qualified Data.Yaml as Y
import Network.Peacock.Type


timeout' :: String -> Int -> IO a -> IO (Maybe a)
timeout' msg tm io = do
    result <- timeout tm io
    maybe (putStrLn msg) (const $ return ()) result
    return result

proxyRequest :: Conf -> ResolvConf -> DNSFormat -> IO (Maybe DNSFormat)
proxyRequest Conf{..} rc req = do
    let worker Resolver{..} = do
            let packet = mconcat . toChunks $ encode req
            sendAll dnsSock packet
            receive dnsSock
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (>>= check) <$> timeout' "proxy timeout" timeOut (worker r)
  where
    ident = identifier . header $ req
    check :: DNSFormat -> Maybe DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Just rsp
                        else Nothing

handleRequest :: Conf -> Connection -> ResolvConf -> DNSFormat -> IO (Maybe DNSFormat)
handleRequest conf conn rc req = do
  mr <- lookupHosts
  case mr of
    Just _ -> return mr
    Nothing -> proxyRequest conf rc req
  where
    filterA = filter ((==A) . qtype)
    ident = identifier . header $ req
    lookupHosts :: IO (Maybe DNSFormat)
    lookupHosts = do
        let mq = listToMaybe . filterA . question $ req
        case mq of
          Just q -> do
            mRecord <- flip runZookeeperPool conn $ getBy (DnsRecordU (qname q))
            case mRecord of
              Just (Entity _key val) ->
                return $ Just $ responseA ident q $ toIPv4 $ dnsRecordAddr val
              Nothing ->
                return Nothing
          Nothing -> return Nothing

handlePacket :: Conf -> Connection -> Socket -> SockAddr -> S.ByteString -> IO ()
handlePacket conf@Conf{..} conn sock addr bs = case decode (fromChunks [bs]) of
    Right req -> do
        let rc = defaultResolvConf { resolvInfo = RCFilePath "/etc/resolv.conf" }
        mrsp <- handleRequest conf conn rc req
        case mrsp of
            Just rsp ->
                let packet = mconcat . toChunks $ encode rsp
                in void $ timeout' "send timeout" timeOut (sendAllTo sock packet addr)
            Nothing -> return ()
    Left msg -> putStrLn msg

runServer :: FilePath -> IO ()
runServer conf | Just conf <- Y.decodeFile "/etc/pocket-dns/zookeeper.yml" = do
  conf' <- Y.parseMonad Database.Persist.loadConfig conf
  withSocketsDo $
    withZookeeperPool conf' $ \conn -> do
      let conf = def
      addrinfos <- getAddrInfo
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing (Just "domain")
      addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
      sock <- socket (addrFamily addrinfo) Datagram defaultProtocol
      bindSocket sock (addrAddress addrinfo)
      forever $ do
          (bs, addr) <- recvFrom sock (bufSize conf)
          forkIO $ handlePacket conf conn sock addr bs
runServer _ = error "Can not parse "
