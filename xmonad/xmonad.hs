import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces(swapTo)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.SetWMName

baseConfig = desktopConfig

main = (xmonad =<<) $ xmobar $ baseConfig
  { terminal = "yakuake"
    , modMask = mod4Mask
    , startupHook = setWMName "LG3D"
  } `additionalKeys`
  [
    ((mod4Mask, xK_bracketleft), moveTo Prev $ Not emptyWS)
  , ((mod4Mask, xK_bracketright), moveTo Next $ Not emptyWS)
  , ((mod4Mask .|. shiftMask, xK_bracketleft),  swapTo Prev)
  , ((mod4Mask .|. shiftMask, xK_bracketright), swapTo Next)
  , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer set Master 2%-")
  , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer set Master 2%+")
  , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
  ]
