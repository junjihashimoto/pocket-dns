{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}

import Data.Monoid
import Test.Sandbox
import Test.Hspec
import qualified Test.Hspec.Sandbox as S
--import Test.Hspec.Contrib.Retry
import Test.Hspec.Server
import Test.Cabal.Path
import qualified Data.Text as T
--import Control.Monad.IO.Class
--import Control.Monad.Trans.Reader
import Text.Shakespeare.Text
import Data.IORef

main :: IO ()
main = withSandbox $ \ref -> do
  bin <- getExePath "." "pocket-dns"
  conf <- newIORef $ error "do not eval this"
  p <- newIORef $ error "do not eval this"
  hspec $ do
    describe "setup(zookeeper)" $ S.with ref $ do
      it "start daemon" $ do
        port' <- getPort "dns"
        liftIO $ writeIORef p port'
        dat <- setFile "conf" $ T.unpack[sbt|backend: zookeeper
                                            |coord: localhost:2181/
                                            |timeout: 300000
                                            |num-stripes: 1
                                            |idletime: 300000
                                            |max-resource: 30
                                            |]
        liftIO $ writeIORef conf dat 
        register "daemon" bin ["daemon","--conf",dat,"--port",show port'] def >>= start
    describe "command-test(zookeeper)" $ with localhost $ do
      it "set" $ do
        dat <- liftIO $ readIORef conf
        command bin ["set","--conf",dat,"hogehoge.","192.168.10.10"] [] @>= exit 0
      it "get" $ do
        dat <- liftIO $ readIORef conf
        command bin ["get","--conf",dat,"hogehoge."] [] @>= exit 0 <> (stdout $ T.unpack [sbt|[192.168.10.10]
                                                                                             |])
      it "dig" $ do
        port' <- liftIO $ readIORef p
        command "dig" ["@localhost","-p",show port',"hogehoge"] [] @>= exit 0
      it "list" $ do
        dat <- liftIO $ readIORef conf
        command bin ["list","--conf",dat] [] @>= exit 0 <> (stdout $ T.unpack [sbt|hogehoge.
                                                                                  |  192.168.10.10
                                                                                  |])
      it "delete" $ do
        dat <- liftIO $ readIORef conf
        command bin ["delete","--conf",dat,"hogehoge."] [] @>= exit 0 <> stdout ""
    describe "setup(sqlite)" $ S.with ref $ do
      it "start daemon(sqlite)" $ do
        port' <- getPort "sdns"
        liftIO $ writeIORef p port'
        dat <- setFile "sconf" $ T.unpack[sbt|backend: sqlite
                                             |database: pocket-dns.sqlite3
                                             |poolsize: 10
                                             |]
        liftIO $ writeIORef conf dat 
        register "sdaemon" bin ["daemon","--conf",dat,"--port",show port'] def >>= start
    describe "command-test(sqlite)" $ with localhost $ do
      it "set" $ do
        dat <- liftIO $ readIORef conf
        command bin ["set","--conf",dat,"hogehoge.","192.168.10.10"] [] @>= exit 0
      it "get" $ do
        dat <- liftIO $ readIORef conf
        command bin ["get","--conf",dat,"hogehoge."] [] @>= exit 0 <> (stdout $ T.unpack [sbt|[192.168.10.10]
                                                                                             |])
      it "dig" $ do
        port' <- liftIO $ readIORef p
        command "dig" ["@localhost","-p",show port',"hogehoge"] [] @>= exit 0
      it "list" $ do
        dat <- liftIO $ readIORef conf
        command bin ["list","--conf",dat] [] @>= exit 0 <> (stdout $ T.unpack [sbt|hogehoge.
                                                                                  |  192.168.10.10
                                                                                  |])
      it "delete" $ do
        dat <- liftIO $ readIORef conf
        command bin ["delete","--conf",dat,"hogehoge."] [] @>= exit 0 <> stdout ""
