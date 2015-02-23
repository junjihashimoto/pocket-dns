{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

--import Prelude hiding (FilePath)
import Network.DNS.Pocket
import Options.Applicative

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import Shelly hiding (command,run,FilePath,get)
import Network.HTTP.Conduit
import Control.Monad
import Control.Concurrent
import System.Exit

default (T.Text)

data Command
  = Set FilePath String [String]
  | Get FilePath String
  | List FilePath
  | Delete FilePath String 
  | Daemon FilePath Port
  deriving Show

set :: Parser Command
set = Set
      <$> option auto (long "conf" <> value "conf.yml" <> metavar "CONFILE")
      <*> (argument str (metavar "DOMAIN"))
      <*> many (argument str (metavar "IP..."))

get :: Parser Command
get = Get
      <$> option auto (long "conf" <> value "conf.yml" <> metavar "CONFILE")
      <*> (argument str (metavar "DOMAIN"))

list :: Parser Command
list = List
       <$> option auto (long "conf" <> value "conf.yml" <> metavar "CONFILE")

delete :: Parser Command
delete = Delete
         <$> option auto (long "conf" <> value "conf.yml" <> metavar "CONFILE")
         <*> (argument str (metavar "DOMAIN"))

daemon :: Parser Command
daemon = Daemon
         <$> option auto (long "conf" <> value "conf.yml" <> metavar "CONFILE")
         <*> option auto (long "port" <> value 53 <> metavar "PORT")

parse :: Parser Command
parse = subparser $ foldr1 (<>) [
        command "set"    (info set (progDesc "set domain and ip"))
      , command "get"    (info get (progDesc "get ip from domain"))
      , command "list"   (info list (progDesc "list domain's ip"))
      , command "delete" (info delete (progDesc "delete domain"))
      , command "daemon" (info daemon (progDesc "start daemon"))
      ]

runCmd :: Command -> IO ()
runCmd (Set conf domain ips) = do
  v <- setDomain conf (B8.pack domain) $ map read ips
  if v
    then print "OK"
    else do
      print "Failed"
      exitWith $ ExitFailure 0

runCmd (Get conf domain) = do
  v <- getDomain conf $ B8.pack domain
  print v

runCmd (List conf) = do
  v <- listDomain conf
  forM_ v $ \(domain,ips) -> do
    B8.putStrLn domain
    forM_ ips $ \ip -> do
      putStr $ "  "
      putStrLn $ show ip

runCmd (Delete conf domain) = do
  deleteDomain conf $ B8.pack domain
    
runCmd (Daemon conf port) = runServer conf port

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd

