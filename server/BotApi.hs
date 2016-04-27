{-# LANGUAGE DataKinds, OverloadedStrings, TypeOperators, RankNTypes, DeriveGeneric, RecordWildCards, ScopedTypeVariables, FlexibleContexts #-}
module BotApi where
import           Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Control.Monad
import           Data.Monoid
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class (liftIO)
import           Servant
import           Data.Word
import           Data.Int
import           System.Directory
import           System.Hardware.Serialport
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
-- import           Data.Aeson.Types (Parser, defaultOptions, fieldLabelModifier)
import           GHC.Generics
import           PID

type CommandChan = B.ByteString
type SerialChan = String
data BotChan = BotChan {
  commands :: TChan CommandChan
, serial :: TChan SerialChan
}

{-
dropParse :: (Generic a, Aeson.GFromJSON (Rep a)) => Aeson.Value -> Parser a
dropParse = Aeson.genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
dropTo :: (Generic a, Aeson.GToJSON (Rep a)) => a -> Aeson.Value
dropTo = Aeson.genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
data ViconLoc = ViconLoc {
  _xvl :: !Int --TODO(MAZUMDER) Word8
, _yvl :: !Int
} deriving (Eq, Show, Generic)
instance Aeson.FromJSON ViconLoc where
  parseJSON = dropParse
instance Aeson.ToJSON ViconLoc where
  toJSON = dropTo
-}
data ViconLoc = ViconLoc {
  xvl :: !Int --TODO(MAZUMDER) Word8
, yvl :: !Int
} deriving (Eq, Show, Generic)
instance Aeson.FromJSON ViconLoc
instance Aeson.ToJSON ViconLoc

type BotAPI =     "forward" :> Get '[] ()
            :<|>  "back" :> Get '[] ()
            :<|>  "cw" :> Get '[] ()
            :<|>  "ccw" :> Get '[] ()
            :<|>  "stop" :> Get '[] ()
            -- :<|>  "location" :> ReqBody '[JSON] ViconLoc :> Post '[JSON] ()
            :<|>  "location" :> Capture "y" Int :> Get '[] ()
            :<|>  "cmps" :> Capture "speed" Int :> Get '[] ()
            :<|>  "rotaterate" :> Capture "r" Double :> Get '[] ()
            :<|>  "rotate" :> Capture "r" Double :> Get '[] ()
            :<|>  "debug" :> Get '[JSON] [FilePath]
            :<|>  "connect" :> Capture "serial" String :> Get '[] ()
            :<|> Raw

app :: (Double -> IO Double) -> BotChan -> Application
app pid bc = serve (Proxy :: Proxy BotAPI) (botapi pid bc)

botapi :: (Double -> IO Double) -> BotChan -> ServerT BotAPI (EitherT ServantErr IO)
botapi pid bc =    store continuouslyforward
          :<|> store continuouslyback
          :<|> store continuouslycw
          :<|> store continuouslyccw
          :<|> store stop
          -- :<|> enter (runReaderTNat bc) locationHandler
          :<|> enter (runReaderTNat bc) locationHandler pid
          :<|> enter (runReaderTNat bc) cmpsHandler
          :<|> enter (runReaderTNat bc) rotateRateHandler
          :<|> enter (runReaderTNat bc) rotateHandler pid
          :<|> debugDevDir
          :<|> connect bc
          :<|> serveDirectory "app.jsexe"
  where store = enter $ runReaderTNat bc

debugDevDir :: EitherT ServantErr IO [FilePath]
debugDevDir = filter (("tty" `L.isInfixOf`) . map C.toLower) <$> liftIO (getDirectoryContents "/dev/")

connect :: BotChan -> String -> EitherT ServantErr IO ()
connect (BotChan _ sc) serial = liftIO . atomically $ writeTChan sc serial

mkDrive :: [Word8] -> B.ByteString
mkDrive = B.pack . (driveOpcode :)

--TODO(MAZUMDER) this nonsense is about as untyped as you can get - promote all constants to singletons
queueCommand :: B.ByteString -> ReaderT BotChan (EitherT ServantErr IO) ()
queueCommand bs = do
  BotChan{..} <- ask
  liftIO . atomically $ writeTChan commands bs

continuouslyforward, continuouslyback, stop, continuouslycw, continuouslyccw :: ReaderT BotChan (EitherT ServantErr IO) ()
continuouslyforward = queueCommand $ mkDrive [0, 150, 128, 0] -- +15cm/s translational
continuouslyback = queueCommand $ mkDrive [255, 106, 128, 0] -- -15cm/s translational
stop = queueCommand $ mkDrive [0, 0, 0, 0] -- TODO(MAZUMDER) this does not match with pycreate
continuouslycw = queueCommand $ mkDrive [0, 67, 255, 255] --  -30 deg/sec rotational
continuouslyccw = queueCommand $ mkDrive [0, 67, 0, 1] --  +30 deg/sec rotational


encodeSpeed :: Int -> BB.Builder
encodeSpeed cmSpeed = BB.int16BE mmSpeed
  where mmSpeed = fromIntegral (cmSpeed * 10)

radToCommand :: Double -> B.ByteString
radToCommand radPerSec
  | radPerSec == 0  = stopcmd
  | velMmSec > 2000 = enc 2000 -- velMmSec can only be positive
  | otherwise       = enc velMmSec
  where stopcmd          = mkDrive [0, 0, 0, 0]
        enc s            = LBS.toStrict . BB.toLazyByteString $ BB.word8 driveOpcode <> encodeMmSec s <> turnInPlace
        velMmSec :: Int  = round (abs radPerSec * 129) -- roomba diameter is 258mm
        ccw      :: Bool = radPerSec >= 0
        encodeMmSec      = BB.int16BE . fromIntegral
        turnInPlace = if ccw then BB.word16BE 0x0001 else BB.word16BE 0xFFFF


cmToCommand :: Int -> IO B.ByteString
cmToCommand speed
  -- | speed < -20 || speed > 20 = print (show speed ++ " too fast") >> stopcmd
  | speed < -50 = enc (-50)
  | speed >  50 = enc   50
  | speed == 0 = stopcmd
  | otherwise = enc speed
  where stopcmd = return $ mkDrive [0, 0, 0, 0]
        enc s = return . LBS.toStrict . BB.toLazyByteString $ BB.word8 driveOpcode <> encodeSpeed s <> BB.word16BE 32768

cmpsHandler :: Int -> ReaderT BotChan (EitherT ServantErr IO) ()
cmpsHandler speed = do
  cmd <- liftIO $ cmToCommand speed
  queueCommand cmd

rotateRateHandler :: Double -> ReaderT BotChan (EitherT ServantErr IO) ()
rotateRateHandler radPerSec = do
  liftIO $ print radPerSec
  liftIO $ print $ round (radPerSec * (258.0/2.0))
  queueCommand $ radToCommand radPerSec

rotateHandler :: (Double -> IO Double) -> Double -> ReaderT BotChan (EitherT ServantErr IO) ()
rotateHandler pid r = do
  liftIO $ print ("error: " ++ show r)
  controlOut <- liftIO $ pid r
  liftIO $ print ("control: " ++ show controlOut)
  queueCommand $ radToCommand controlOut

-- locationHandler :: ViconLoc -> ReaderT BotChan (EitherT ServantErr IO) ()
-- locationHandler v@ViconLoc{..} = do
--   liftIO $ print v
locationHandler :: (Double -> IO Double) -> Int -> ReaderT BotChan (EitherT ServantErr IO) ()
locationHandler pid y = do
  -- let pid :: _ = givesetpoint 0
  liftIO $ print ("vicon: " ++ show y)
  controlOut <- liftIO $ pid (fromIntegral (-y))
  liftIO $ print ("control: " ++ show controlOut)
  cmd <- liftIO $ cmToCommand (round controlOut)
  queueCommand cmd

startOpcode, safeOpcode, driveOpcode :: Word8
startOpcode = 128
safeOpcode = 131
driveOpcode = 137

serialx :: BotChan -> IO () --TODO(MAZUMDER) switch loop to Pipes
serialx (BotChan commandchan serialchan) = do
  kickoff <- async (return ())
  loop kickoff
  where loop :: Async () -> IO ()
        loop old = do
          s :: String <- ("/dev/" ++) <$> atomically (readTChan serialchan)
          print $ "connecting to " ++ s
          cancel old
          atomically $ drain commandchan
          new <- async (bot s)
          -- new <- async (dbg s)
          loop new
        bot :: String -> IO ()
        bot s = withSerial s defaultSerialSettings { commSpeed = CS57600 } $ \h -> do
          _ <- send h $ B.singleton startOpcode
          _ <- send h $ B.singleton safeOpcode
          forever $ do
            c <- atomically $ readTChan commandchan
            send h c
        drain :: TChan CommandChan -> STM () -- if the bot thread dies (loses serial connection) then drain any commands before opening a new serial connection
        drain ch = do
          e <- isEmptyTChan ch
          unless e $ readTChan ch >> drain ch
        -- dbg :: String -> IO ()
        -- dbg s = do
        --   print s
        --   forever $ do
        --     c <- atomically $ readTChan commandchan
        --     print c

main :: IO ()
main = do
  cmds <- atomically newTChan
  ser <- atomically newTChan
  let botchan = BotChan cmds ser
  _ <- async $ serialx botchan -- TODO(MAZUMDER) clean teardown with withAsync
  pid <- pidTimedIO 0.5 0 0
  run 9876 $ app (pid 0) botchan
