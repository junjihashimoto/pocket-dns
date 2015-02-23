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
import Database.Persist.Sqlite (SqliteConf)
import Database.Persist.Zookeeper (ZookeeperConf)
import Network.DNS.Pocket.Type
import Network.DNS.Pocket.Zookeeper ()
import Network.DNS.Pocket.Sqlite ()

type Port = Int

timeout' :: String -> Int -> IO a -> IO (Maybe a)
timeout' msg tm io = do
    result <- timeout tm io
    maybe (putStrLn msg) (const $ return ()) result
    return result

proxyRequest :: DnsConf -> ResolvConf -> DNSFormat -> IO (Maybe DNSFormat)
proxyRequest DnsConf{..} rc req = do
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

handleRequest :: DnsBackend c => c -> DnsConf -> Conn c -> ResolvConf -> DNSFormat -> Int -> IO (Maybe DNSFormat)
handleRequest pconf conf conn rc req n = do
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
            val <- getRecord pconf (qname q) conn
            let len = length val
            if len /= 0
              then 
                case (val !! (n `div` len)) of
                  IPv4 ip -> return $ Just $ responseA ident q ip
                  IPv6 ip -> return $ Just $ responseAAAA ident q ip
              else
                return Nothing
          Nothing -> return Nothing

handlePacket :: DnsBackend c => c -> DnsConf -> Conn c -> Socket -> SockAddr -> S.ByteString -> Int -> IO ()
handlePacket pconf conf@DnsConf{..} conn sock addr bs n = case decode (fromChunks [bs]) of
    Right req -> do
        let rc = defaultResolvConf { resolvInfo = RCFilePath "/etc/resolv.conf" }
        mrsp <- handleRequest pconf conf conn rc req n
        case mrsp of
            Just rsp ->
                let packet = mconcat . toChunks $ encode rsp
                in void $ timeout' "send timeout" timeOut (sendAllTo sock packet addr)
            Nothing -> return ()
    Left msg -> putStrLn msg


runServer' :: DnsBackend c => c -> Port -> IO ()
runServer' conf port = do
  withSocketsDo $ do
    mconn <- setup conf
    case mconn of
      Just conn -> do
        addrinfos <- getAddrInfo
                       (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                       Nothing (Just "domain")
        addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
        addrinfo' <- do
          if port == 53
            then return addrinfo
            else do
              let sockaddr = addrAddress addrinfo
              let sockaddr' = case sockaddr of
                    SockAddrInet _ addr -> SockAddrInet (fromIntegral port) addr
                    SockAddrInet6 _ f h s -> SockAddrInet6 (fromIntegral port) f h s
                    SockAddrUnix _ -> sockaddr
              return $ addrinfo { addrAddress = sockaddr' }
        sock <- socket (addrFamily addrinfo') Datagram defaultProtocol
        bindSocket sock (addrAddress addrinfo')
        loop sock conn def 0
      Nothing -> error $ "Setup failure"
  where
    loop sock conn def' n = do
      (bs, addr) <- recvFrom sock (bufSize def')
      _ <- forkIO $ handlePacket conf def' conn sock addr bs n
      case n of
        255 -> loop sock conn def' 0
        _ -> loop sock conn def' (n+1)

loadConf :: FilePath -> IO (Maybe Conf)
loadConf yamlConfFile = do
  mconf <- load yamlConfFile :: IO (Maybe ZookeeperConf)
  case mconf of
    Just conf -> return $ Just $ Zookeeper conf
    Nothing -> do
      mconf <- load yamlConfFile :: IO (Maybe SqliteConf)
      case mconf of
        Just conf -> return $ Just $ Sqlite conf
        Nothing -> return Nothing 

runServer :: FilePath -> Port -> IO ()
runServer file port = do
  mconf <- loadConf file
  case mconf of
    Just (Zookeeper conf) -> runServer' conf port
    Just (Sqlite conf) -> runServer' conf port
    Nothing -> error "Can not parse config-file"


setDomain :: FilePath -> Domain -> [IP] -> IO Bool
setDomain file domain ips = do
  mconf <- loadConf file
  case mconf of
    Just (Zookeeper conf) -> setDomain' conf domain ips
    Just (Sqlite conf) -> setDomain' conf domain ips
    Nothing -> error "Can not parse config-file"
  where
    setDomain' :: DnsBackend c => c -> Domain -> [IP] -> IO Bool
    setDomain' conf domain ips = do
      withSocketsDo $ do
        mconn <- setup conf
        case mconn of
          Just conn -> setRecord conf domain ips conn
          Nothing -> error "Setup failure"

getDomain :: FilePath -> Domain -> IO [IP]
getDomain file domain = do
  mconf <- loadConf file
  case mconf of
    Just (Zookeeper conf) -> getDomain' conf domain
    Just (Sqlite conf) -> getDomain' conf domain
    Nothing -> error "Can not parse config-file"
  where
    getDomain' :: DnsBackend c => c -> Domain -> IO [IP]
    getDomain' conf domain = do
      withSocketsDo $ do
        mconn <- setup conf
        case mconn of
          Just conn -> getRecord conf domain conn
          Nothing -> error "Setup failure"

deleteDomain :: FilePath -> Domain -> IO ()
deleteDomain file domain = do
  mconf <- loadConf file
  case mconf of
    Just (Zookeeper conf) -> deleteDomain' conf domain
    Just (Sqlite conf) -> deleteDomain' conf domain
    Nothing -> error "Can not parse config-file"
  where
    deleteDomain' :: DnsBackend c => c -> Domain -> IO ()
    deleteDomain' conf domain = do
      withSocketsDo $ do
        mconn <- setup conf
        case mconn of
          Just conn -> deleteRecord conf domain conn
          Nothing -> error "Setup failure"


listDomain :: FilePath -> IO [(Domain,[IP])]
listDomain file = do
  mconf <- loadConf file
  case mconf of
    Just (Zookeeper conf) -> listDomain' conf
    Just (Sqlite conf) -> listDomain' conf
    Nothing -> error "Can not parse config-file"
  where
    listDomain' :: DnsBackend c => c -> IO [(Domain,[IP])]
    listDomain' conf = do
      withSocketsDo $ do
        mconn <- setup conf
        case mconn of
          Just conn -> listRecord conf conn
          Nothing -> error "Setup failure"

