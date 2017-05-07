import XMonad
import XMonad.Config.Desktop
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces(swapTo)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86

baseConfig = desktopConfig

main = xmonad $ baseConfig
  { terminal = "yakuake"
    , modMask = mod4Mask
  } `additionalKeys`
  [
    ((mod4Mask, xK_bracketleft), moveTo Prev NonEmptyWS)
  , ((mod4Mask, xK_bracketright), moveTo Next NonEmptyWS)
  , ((mod4Mask .|. shiftMask, xK_bracketleft),  swapTo Prev)
  , ((mod4Mask .|. shiftMask, xK_bracketright), swapTo Next)
  , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
  , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
  , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
  ]
