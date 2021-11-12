
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.X11.Xlib.Types (Display)
import XMonad
import XMonad.Core (XConf)
import XMonad.Config ()
import XMonad.StackSet (shift, greedyView, RationalRect(..))

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook, fadeWindowsLogHook, opaque, transparency, Opacity)

import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.LayoutScreens (layoutScreens,layoutSplitScreen)
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Grid

import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Wallpaper

import Data.Ratio((%))
import Data.Monoid
import Data.Default ()

modM :: KeyMask
modM   = mod4Mask
borderC :: [Char]
borderC = "black"
focusedBorderC :: [Char]
focusedBorderC = "white"

browser :: X ()
browser = spawn "pgrep chrome || google-chrome"
chat :: X ()
chat    = spawn "slack"
term :: [Char]
term    = "pgrep gnome-terminal || gnome-terminal"
emacs :: X ()
emacs   = spawn "pgrep emacs || emacs -l ~/workdir/dot-emacs/init.el"
code :: X ()
code    = spawn "pgrep code || code"
virt :: X ()
virt    = spawn "virt-manager"
remote :: X ()
remote  = spawn "vinagre"
logout :: X ()
logout  = spawn "loginctl kill-session $XDG_SESSION_ID"
screenshot :: X ()
screenshot = spawn "gnome-screenshot -i"
gnomeSession :: Display -> X ()
gnomeSession display = spawn $ "pkill Xephyr; Xephyr -resizeable -fullscreen :"++nd++"& DISPLAY=:"++nd++" gnome-session"
  where
    nd = (show . (1+) . read . drop 1 . displayString) display

myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
               [ className =? "Chromium" --> doShift "1:browser"
               , className =? "Slack" --> doShift "2:chat"
               , className =? "Xfce4-terminal" --> doShift "3:term"
               , className =? "Gnome-terminal" --> doShift "3:term"
               , className =? "Code" --> doShift "4:code"
               , className =? "Emacs" --> doShift "5:emacs"
               , className =? "Virt-manager" --> doShift "6:virt"
               , className =? "VirtualBox Manager" --> doShift "6:virt"
               , className =? "Vinagre" --> doShift "7:remote"
               , className =? "Thunar" --> doShift "8:file"
               , className =? "Nautilus" --> doShift "8:file"
               , className =? "Xephyr" --> doShift "9:gnome"
               , title     =? "Whisker Menu" --> doFloat
               , className =? "Gnome-control-center" --> doFloat
               , className =? "Wine" --> doFloat
               , className =? "xfce4-notes" --> doFloat
               , className =? "Weston Compositor" --> doFloat
               , className =? "Gnome-screenshot" --> doFloat
               -- , className =? "Google-chrome" --> doRectFloat (RationalRect ((1920 - 1368) % (1920 * 2)) ((1080 - 768) % (1080 * 2)) (1368 % 1920) (768 % 1080))
               , className =? "Update-manager" --> doRectFloat (RationalRect (1 % 4) (1 % 3) (1 % 2) (1 % 2)) -- doCenterFloat
               , className =? "Xfrun4" --> doRectFloat (RationalRect (1 % 4) (1 % 3) (1 % 2) (1 % 2)) -- doCenterFloat
               , manageDocks
               ]

myFadeHook :: Query Opacity
myFadeHook = composeAll [isUnfocused --> transparency 0.2
                        ,                opaque
                        ]

myStartupHook :: X ()
myStartupHook = do
  layoutScreens 2 myLayout
  -- spawn "xmobar -x 1"
  -- spawn "xcompmgr -c"
  XConf{display} <- ask
  -- spawn ("zenity --info --text="++displayString display)
  spawn term
  browser
  chat
  code
  emacs
  gnomeSession display

myLayout = layoutTwoPane -- ||| layoutSpiral ||| layoutTall ||| layoutMirror ||| layoutSpiral ||| layoutGrid ||| layoutThreeCol ||| layoutFull
    where
      layoutTall = Tall 1 (3/100) (1/2)
      layoutSpiral = spiral (125 % 146)
      layoutGrid = Grid
      layoutMirror = Mirror (Tall 1 (3/100) (3/5))
      layoutTwoPane = TwoPane 0.5 0.5
      layoutThreeCol = ThreeColMid 1 (3/100) (1/2)
      layoutFull = Full

myXmobar = statusBar "xmobar" (xmobarPP { ppLayout = const "", ppTitle = const "" }) toggleStrutsKey
  where
    toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main :: IO ()
main = do
        setRandomWallpaper ["$HOME/Pictures/Wallpapers"]
        xmonad =<< myXmobar config
  where
    config = docks def
      { workspaces = ["1:browser", "2:chat", "3:term", "4:code", "5:emacs", "6:virt", "7:remote", "8:file", "9:gnome"]
      , manageHook = myManageHook <+> manageHook def
      , layoutHook = avoidStruts $ onWorkspace "float" simplestFloat $ layoutHook def
      , logHook = fadeWindowsLogHook myFadeHook
      , startupHook = myStartupHook <+> startupHook def
      , handleEventHook = fadeWindowsEventHook
      , modMask  = modM
      , terminal = term
      , normalBorderColor = borderC
      , focusedBorderColor = focusedBorderC
      , focusFollowsMouse = False
      }
      `additionalKeys`
      ([
        ((modM,               xK_KP_1), browser)
      , ((modM,               xK_KP_2), chat)
      , ((modM,               xK_KP_4), emacs)
      , ((modM,               xK_KP_5), code)
      , ((modM,               xK_KP_6), virt)
      , ((modM,               xK_KP_7), remote)
      , ((modM,               xK_s   ), screenshot)
      , ((modM .|. shiftMask, xK_q   ), logout)
      , ((modM .|. shiftMask, xK_b   ), sendMessage $ ToggleStrut U)
      , ((modM .|. shiftMask, xK_space), layoutScreens 2 myLayout)
      , ((modM .|. controlMask .|. shiftMask, xK_space), rescreen)
      ] ++ [((m .|. modM, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e] [0..1]
            , (f, m) <- [(greedyView, 0), (shift, shiftMask)]
            ]
      )
