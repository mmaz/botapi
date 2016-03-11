{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, RankNTypes, RecordWildCards, ScopedTypeVariables #-}
module BotApi where
import           Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Control.Monad
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import           Servant
import           Data.Word
import           System.Directory
-- import           System.Hardware.Serialport
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.ByteString as B

-- type CommandChan = B.ByteString
type CommandChan = String
type SerialChan = String
data BotChan = BotChan {
  commands :: TChan CommandChan
, serial :: TChan SerialChan
}

type BotAPI =     "forward" :> Get '[] ()
            :<|>  "back" :> Get '[] ()
            :<|>  "cw" :> Get '[] ()
            :<|>  "ccw" :> Get '[] ()
            :<|>  "stop" :> Get '[] ()
            :<|>  "debug" :> Get '[JSON] [FilePath]
            :<|>  "connect" :> Capture "serial" String :> Get '[] ()
            :<|> Raw

app :: BotChan -> Application
app bc = serve (Proxy :: Proxy BotAPI) (botapi bc)

botapi :: BotChan -> ServerT BotAPI (EitherT ServantErr IO)
botapi bc =    store continuouslyforward
          :<|> store continuouslyback
          :<|> store continuouslycw
          :<|> store continuouslyccw
          :<|> store stop
          :<|> debugDevDir
          :<|> connect bc
          :<|> serveDirectory "app.jsexe"
  where store = enter $ runReaderTNat bc

debugDevDir :: EitherT ServantErr IO [FilePath]
debugDevDir = filter (("tty" `L.isInfixOf`) . map C.toLower) <$> liftIO (getDirectoryContents "/dev/")

connect :: BotChan -> String -> EitherT ServantErr IO ()
connect (BotChan _ sc) serial = liftIO . atomically $ writeTChan sc serial

mkDrive :: [Word8] -> B.ByteString
mkDrive = B.concat . map B.singleton . (driveOpcode :)

--TODO(MAZUMDER) this nonsense is about as untyped as you can get - promote all constants to singletons
queueCommand :: String -> ReaderT BotChan (EitherT ServantErr IO) ()
queueCommand bs = do
  BotChan{..} <- ask
  liftIO . atomically $ writeTChan commands bs

continuouslyforward, continuouslyback, stop, continuouslycw, continuouslyccw :: ReaderT BotChan (EitherT ServantErr IO) ()
continuouslyforward = queueCommand "forward" --  mkDrive [0, 150, 128, 0] -- +15cm/s translational
continuouslyback = queueCommand "backward" --  mkDrive [255, 106, 128, 0] -- -15cm/s translational
stop = queueCommand "stop" -- mkDrive [0, 0, 0, 0] -- TODO(MAZUMDER) this does not match with pycreate
continuouslycw = queueCommand "cw" --  mkDrive [0, 67, 255, 255] --  -30 deg/sec rotational
continuouslyccw = queueCommand "ccw" -- mkDrive [0, 67, 0, 1] --  +30 deg/sec rotational

startOpcode, safeOpcode, driveOpcode :: Word8
startOpcode = 128
safeOpcode = 131
driveOpcode = 137

serialx :: BotChan -> IO () --TODO(MAZUMDER) switch loop to Pipes
serialx (BotChan commandchan serialchan) = do
  kickoff <- async (atomically $ writeTChan serialchan "debug")
  loop kickoff
  where loop :: Async () -> IO ()
        loop old = do
          s :: String <- ("/dev/" ++) <$> atomically (readTChan serialchan)
          print $ "connecting to " ++ s
          cancel old
          atomically $ drain commandchan
          -- new <- async (bot s)
          new <- async (dbg s)
          loop new
        -- bot :: String -> IO ()
        -- bot s = withSerial s defaultSerialSettings { commSpeed = CS57600 } $ \h -> do
        --   _ <- send h $ B.singleton startOpcode
        --   _ <- send h $ B.singleton safeOpcode
        --   forever $ do
        --     c <- atomically $ readTChan commandchan
        --     send h c
        drain :: TChan CommandChan -> STM () -- if the bot thread dies (loses serial connection) then drain any commands before opening a new serial connection
        drain ch = do
          e <- isEmptyTChan ch
          unless e $ readTChan ch >> drain ch
        dbg :: String -> IO ()
        dbg s = do
          print s
          forever $ do
            c <- atomically $ readTChan commandchan
            print c

main :: IO ()
main = do
  cmds <- atomically newTChan
  ser <- atomically newTChan
  let botchan = BotChan cmds ser
  _ <- async $ serialx botchan -- TODO(MAZUMDER) clean teardown with withAsync
  run 9878 $ app botchan
