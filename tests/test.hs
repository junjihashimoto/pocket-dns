import Data.Monoid
import Test.Hspec
import Test.Hspec.Server
import Test.Hspec.Contrib.Retry
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

main :: IO ()
main = hspec $ do
    describe "hoge" $ with localhost $ do
      it "check os" $ do
        os <- getServerOS
        liftIO $ os `shouldBe` (Just $ Ubuntu "12.04")
      it "process test" $ do
        process "java" @>= running
        process "hoge" @>= none
      it "package test" $ do
        package "zookeeper" @>= installed
        liftIO $ do
          runReaderT (package "zookeeper-hoge") localhost `shouldThrow` anyException  -- Handling exception is not good. Should change
      it "port test" $ do
        port 2181 @>= listening
        port 2180 @>= none
      it "service test" $ do
        service "zookeeper" @>= running
        liftIO $ do
          runReaderT (service "zookeeper-hoge") localhost `shouldThrow` anyException  -- Handling exception is not good. Should change
      it "command test" $ do
        command "bash" ["-c","exit 2"] [] @>= exit 2
        command "echo" ["hoge"] [] @>= exit 0 <> stdout "hoge\n"
        v <- command "echo" ["hoge"] []
        v `includes'` (exit 0 <> stdout "hoge\n")
        liftIO $ getStdout v `shouldBe` Just "hoge\n"
        liftIO $ getStderr v `shouldBe` Just ""
      it "retry test" $ do
        retryWith 10 $
          command "ls" [] [] @>= exit 0
      it "network" $ do
        host "localhost" @>= reachable
        host "localhost" @== reachable
        hostWithPort "localhost" 2181 @>= reachable
        host "hogehoge" @>= none
        hostWithPort "hogehoge" 2181 @>= none
