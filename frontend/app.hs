{-# LANGUAGE RecursiveDo, OverloadedStrings, TupleSections, ScopedTypeVariables, ForeignFunctionInterface, JavaScriptFFI, CPP #-}
import           Reflex
import           Reflex.Dom
import           Control.Applicative
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.IO.Class
import           Data.Map (Map)
import           Data.Monoid
import           GHCJS.DOM.Types (Element, unElement)
import           GHCJS.DOM.Element (toElement)
import           GHCJS.Prim (JSRef)

data Discrete = Down | Up deriving (Eq)

discreteButton :: MonadWidget t m => String -> m (Event t Discrete)
discreteButton btntext = do
  let attrs = "class" =: "waves-effect waves-light purple accent-2 btn"
  (btnEl, _) <- elAttr' "button" attrs $ text btntext
  return $ leftmost [const Down <$> domEvent Mousedown btnEl, const Up <$> domEvent Mouseup btnEl, const Up <$> domEvent Mouseleave btnEl] -- NB: Mouseout is *not* what you want

data BotAPI = BotAPI String String
forward, back, cw, ccw, stop :: BotAPI
forward = BotAPI "↑" "forward"
back = BotAPI "↓" "back"
cw = BotAPI "⟳" "cw"
ccw = BotAPI "⟲" "ccw"
stop = BotAPI "⇏" stopurl
stopurl :: String
stopurl = "stop"

controlRoomba :: MonadWidget t m => BotAPI -> m ()
controlRoomba (BotAPI icon url) = do
  evDis <- discreteButton icon
  let start = xhrRequest "GET" url def
  let end = xhrRequest "GET" stopurl def
  performRequestAsync $ const start <$> ffilter (== Down) evDis
  performRequestAsync $ const end <$> ffilter (== Up) evDis
  blank

rotateButton :: MonadWidget t m => String -> m (Event t ())
rotateButton btntext = do
  ct <- liftIO getCurrentTime
  let attrs = "class" =: "waves-effect waves-light pink btn-large"
  (rotateEl, _) <- elAttr' "button" attrs $ text btntext
  let downEv = leftmost [const True <$> domEvent Mousedown rotateEl, const False <$> domEvent Mouseup rotateEl, const False <$> domEvent Mouseleave rotateEl] -- NB: Mouseout is *not* what you want
  downBehavior <- hold False downEv
  timestream <- tickLossy 0.05 ct
  return $ const () <$> gate downBehavior timestream

wrapangle :: Int -> Int
wrapangle ang
  | 0 <= ang && ang <= 359   = ang
  | ang < 0                  = wrapangle $ ang + 360
  | otherwise                = ang `mod` 360

applyrotation :: (Int -> Int) -> Int -> Int
applyrotation f = wrapangle . (foldr (.) id $ replicate 4 f)

-- https://developer.mozilla.org/en-US/docs/Web/CSS/transform
-- https://developer.mozilla.org/en-US/docs/Web/CSS/transform-function
-- https://developer.mozilla.org/en-US/docs/Web/Guide/Events/Using_device_orientation_with_3D_transforms
-- https://developer.mozilla.org/en-US/docs/Web/CSS/angle
mkRot :: MonadWidget t m => Dynamic t Int -> m (Dynamic t (Map String String))
mkRot = mapDyn (\i -> "style" =: mkTrans (show i))
  where basecss = "width: 500px; margin: 0 auto; background-image: url(\"roomba.png\"); background-size: 500px; "
        mkTrans :: String -> String
        mkTrans d = basecss ++ "transform: rotate(" ++ d ++ "deg); transform-origin: 50% 50%;" --TODO(MAZUMDER) I guess spans need a fixed-width for rotations?

roomba :: MonadWidget t m => Dynamic t (Map String String) -> m ()
roomba rot = do
  -- let img = "src" =: "roomba.png" -- <> "width" =: "500px"
  elDynAttr "div" rot $ elAttr "div" ("style" =: "font-size: 50px; width: 500px; height: 500px;") $ do
    divClass "container" $ do
      divClass "row" $ divClass "col s12 center-align" $ divClass "chip" $ text "irobot create"
      divClass "row" $ divClass "col s12 center-align" $ controlRoomba forward
      divClass "row" $ divClass "col s12 center-align" $ controlRoomba stop
      divClass "row" $ do
        divClass "col s6 center-align" $ controlRoomba ccw
        divClass "col s6 center-align" $ controlRoomba cw
      divClass "row" $ divClass "col s12 center-align" $ controlRoomba back

connect :: MonadWidget t m => m ()
connect = divClass "row" $ do
  let a1 = "class" =: "waves-effect waves-light green btn"
  (b1, _) <- elAttr' "button" a1 $ text "/dev/ttyUSB0"
  let conn = xhrRequest "GET" "connect/ttyUSB0" def
  performRequestAsync $ pure conn <$> (domEvent Click b1)
  let a2 = "class" =: "waves-effect waves-light orange btn"
  (b2, _) <- elAttr' "button" a2 $ text "/dev/ttyUSB1"
  let conn = xhrRequest "GET" "connect/ttyUSB1" def
  performRequestAsync $ pure conn <$> (domEvent Click b2)
  blank

main :: IO ()
main = mainWidget $ do
  dynrotcss <- elAttr "div" ("style" =: "width: 250px; margin: 0 auto;") $ do
    rev <- rotateButton "⟲"
    lev <- elAttr "span" ("style" =: "float: right;") $ rotateButton "⟳"
    dynr <- foldDyn (const (applyrotation pred)) 0 rev
    dynval <- joinDyn <$> foldDynM (\() dr -> mapDyn (applyrotation succ) dr) dynr lev
    mkRot dynval
  roomba dynrotcss
  elAttr "div" ("style" =: "width: 400px; margin: 0 auto;") $ connect
  blank
