{-# LANGUAGE QuasiQuotes #-}

---------------------------------------------------------------------------
--                                                                       --
--     _|      _|  _|      _|                                      _|    --
--       _|  _|    _|_|  _|_|    _|_|    _|_|_|      _|_|_|    _|_|_|    --
--         _|      _|  _|  _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--       _|  _|    _|      _|  _|    _|  _|    _|  _|    _|  _|    _|    --
--     _|      _|  _|      _|    _|_|    _|    _|    _|_|_|    _|_|_|    --
--                                                                       --
---------------------------------------------------------------------------
-- Marek Fajkus <marek.faj@gmail.com> @turbo_MaCk                        --
-- https://github.com/turboMaCk                                          --
---------------------------------------------------------------------------
import XMonad hiding (doFloat)
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (
    ewmh,
    ewmhFullscreen,
 )
import qualified XMonad.Hooks.ManageDocks as Docks
import XMonad.Hooks.ManageHelpers (doFullFloat, doRectFloat, isFullscreen)
import qualified XMonad.Hooks.SetWMName as WMName
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Simplest (Simplest (..))
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.WorkspaceDir (changeDir, workspaceDir)

import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Prompt
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified Codec.Binary.UTF8.String as UTF8

import qualified Data.Map as M
import Data.Monoid (Endo)
import System.Exit (
    ExitCode (ExitSuccess),
    exitWith,
 )
import Text.RawString.QQ

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Actions.Volume as Volume
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration (shrinkText)
import qualified XMonad.Layout.Fullscreen as F
import XMonad.Layout.MouseResizableTile (mouseResizableTile)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts
import XMonad.Layout.ThreeColumns (ThreeCol (..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.ManageHook as ManageHook
import qualified XMonad.Util.Brightness as Brightness

-------------------------------------
-- Main
-------------------------------------

main :: IO ()
main = xmonad $ Docks.docks $ ewmhFullscreen $ ewmh $ myConfig

-------------------------------------
-- Config
-------------------------------------

myTerminal :: String
myTerminal = "alacritty"

red = "#fb4934"
bg1 = "#3c3836"
bg2 = "#504945"
blue = "#039be5"
black = "#000000"

-- Main configuration, override the defaults to your liking.
myConfig =
    def
        { modMask = mod4Mask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , keys = myKeys
        , layoutHook = myLayoutHook
        , focusedBorderColor = blue
        , normalBorderColor = black
        , manageHook =
            myManageHook
                <+> manageHook def
                <+> manageScratchPad
                <+> F.fullscreenManageHook
        , borderWidth = 4
        , startupHook = myStartupHook
        }

-- then define your scratchpad management separately:
manageScratchPad :: ManageHook
manageScratchPad = NS.namedScratchpadManageHook scratchpads

scratchpads =
    [ NS.NS "obs" "obs" (className =? ".obs-wrapped") doFloat
    , NS.NS "peek" "peek" (className =? "Peek") doFloat
    , NS.NS "slack" "slack" (className =? "slack") doFloat
    , NS.NS "discord" "discord" (className =? "discord") doFloat
    , NS.NS "keybase-gui" "keybase-gui" (className =? "Keybase") doFloat
    ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x = [x]

webW = "1"
communicateW = "2"
emailW = "3"
codeW = "4"
musicW = "8"
virtualW = "9"

myWorkspaces :: [String]
myWorkspaces =
    clickable . (map xmobarEscape) $
        [ webW
        , communicateW
        , emailW
        , codeW
        , "5"
        , "6"
        , "7"
        , musicW
        , virtualW
        ]
  where
    clickable l =
        [ ws
        | (i, ws) <- zip [1 .. 10] l
        , let n = i
        ]

-------------------------------------
-- Keys
-------------------------------------

rofi :: String
rofi =
    [r| rofi -modi drun,run -show drun -theme "$HOME/Dotfiles/rofi/launcher.rasi" |]

floatRect :: W.RationalRect
floatRect = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (4 / 5)

toggleFloat :: Window -> X ()
toggleFloat w = windows $
    \s ->
        if M.member w (W.floating s)
            then W.sink w s
            else (W.float w floatRect s)

-- Overide Xmonad's `doFloat`
doFloat :: ManageHook
doFloat = doRectFloat floatRect

myKeys conf@(XConfig{XMonad.modMask = modMasq}) =
    M.fromList $
        -- launch a terminal
        [ ((modMasq .|. shiftMask, xK_t), spawn myTerminal)
        , -- set directory
          ((modMasq .|. shiftMask, xK_x), changeDir def)
        , -- launch a browser
          ((modMasq .|. shiftMask, xK_b), spawn "firefox")
        , -- launch emacs client frame
          ((modMasq .|. shiftMask, xK_o), spawn "emacsclient -n -c $PWD")
        , -- screenshot
          ((modMasq .|. shiftMask, xK_p), spawn "flameshot gui")
        , -- launch rofi
          ((modMasq, xK_p), spawn rofi)
        , -- launch fori-pass
          ((modMasq, xK_o), spawn "rofi-pass")
        , -- launch lf.
          ((modMasq .|. shiftMask, xK_m), spawn $ myTerminal ++ " -e lf")
        , -- close focused window
          ((modMasq, xK_q), kill)
        , -- Rotate through the available layout algorithms
          ((modMasq, xK_space), sendMessage NextLayout)
        , --  Reset the layouts on the current workspace to default
          ((modMasq .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
        , -- Resize viewed windows to the correct size
          ((modMasq, xK_n), refresh)
        , -- Move focus to the next window
          ((modMasq, xK_Tab), focusDown)
        , -- Move focus to the next window
          ((modMasq, xK_j), focusDown)
        , -- Move focus to the previous window
          ((modMasq, xK_k), focusUp)
        , -- Move focus to the master window
          ((modMasq, xK_m), windows W.focusMaster)
        , ((modMasq, xK_a), windows copyToAll)
        , -- Swap the focused window and the master window
          ((modMasq, xK_Return), windows W.swapMaster)
        , -- swap the focused window with the next window
          ((modMasq .|. shiftMask, xK_j), windows W.swapDown)
        , -- Swap the focused window with the previous window
          ((modMasq .|. shiftMask, xK_k), windows W.swapUp)
        , -- Shrink the master area
          ((modMasq, xK_h), sendMessage Shrink)
        , -- Expand the master area
          ((modMasq, xK_l), sendMessage Expand)
        , -- Push window back into tiling
          ((modMasq, xK_t), withFocused toggleFloat)
        , -- Increment the number of windows in the master area
          ((modMasq, xK_comma), sendMessage (IncMasterN 1))
        , -- Deincrement the number of windows in the master area
          ((modMasq, xK_period), sendMessage (IncMasterN (-1)))
        , -- Marks as booring
          ((modMasq, xK_i), markBoring)
        , ((modMasq, xK_u), clearBoring)
        , -- Quit xmonad
          ((modMasq .|. shiftMask, xK_0), io (exitWith ExitSuccess))
        , -- Sratchpads
          ((modMasq .|. shiftMask, xK_f), NS.namedScratchpadAction scratchpads "slack")
        , ((modMasq .|. shiftMask, xK_d), NS.namedScratchpadAction scratchpads "discord")
        , ((modMasq .|. shiftMask, xK_g), NS.namedScratchpadAction scratchpads "peek")
        , ((modMasq .|. shiftMask, xK_h), NS.namedScratchpadAction scratchpads "obs")
        , ((modMasq .|. shiftMask, xK_s), NS.namedScratchpadAction scratchpads "keybase-gui")
        , -- Struts...
          ((modMasq, xK_b), sendMessage $ Docks.ToggleStrut Docks.U)
        , -- Restart xmonad
          ((modMasq .|. shiftMask, xK_q), spawn "xmonad --recompile; killall polybar; notify-send \"XMonad\" \"Reloaded!\"; xmonad --restart")
        , -- TODO: review
          ((modMasq .|. controlMask, xK_h), sendMessage $ pullGroup L)
        , ((modMasq .|. controlMask, xK_l), sendMessage $ pullGroup R)
        , ((modMasq .|. controlMask, xK_k), sendMessage $ pullGroup U)
        , ((modMasq .|. controlMask, xK_j), sendMessage $ pullGroup D)
        , ((modMasq .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
        , ((modMasq .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
        , ((modMasq .|. shiftMask, xK_comma), onGroup W.focusUp')
        , ((modMasq .|. shiftMask, xK_period), onGroup W.focusDown')
        , ((modMasq, xK_z), sendMessage MirrorShrink)
        , ((modMasq, xK_s), sendMessage MirrorExpand)
        , -- TODO: review
          -- Media

          -- VOLUME
          ((0, xF86XK_AudioMute), Volume.toggleMute >> pure ())
        , ((0, xF86XK_AudioLowerVolume), Volume.lowerVolume 10 >> pure ())
        , ((0, xF86XK_AudioRaiseVolume), Volume.raiseVolume 10 >> pure ())
        , ((0, xF86XK_AudioMicMute), spawn micMuteToggleScript)
        , -- Brightness
          ((0, xF86XK_MonBrightnessUp), Brightness.increase)
        , ((0, xF86XK_MonBrightnessDown), Brightness.decrease)
        ]
            ++
            --
            -- mod-[1..9], Switch to workspace N
            --
            -- mod-[1..9], Switch to workspace N
            -- mod-shift-[1..9], Move client to workspace N
            --
            [ ( (m .|. modMasq, k)
              , windows $ f i
              )
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
            ]
            ++
            --
            -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
            -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
            --
            [ ((m .|. modMasq, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
            ]

micMuteToggleScript :: String
micMuteToggleScript =
    unlines
        [ "if [[ $(amixer sget Capture) == *\"[on]\"* ]]; then"
        , "  amixer sset Capture nocap"
        , "else"
        , "  amixer sset Capture cap"
        , "fi"
        ]

-------------------------------
-- spawn processes
-------------------------------

myStartupHook :: X ()
myStartupHook = do
    spawn "feh --bg-center ~/Dotfiles/images/wallpaper.jpg"
    spawn "polybar main"
    -- Java Apps might require this
    WMName.setWMName "LG3D"

myManageHook :: Query (Endo WindowSet)
myManageHook =
    composeAll
        [ Docks.manageDocks
        , isFullscreen --> doF W.focusDown <+> doFullFloat
        , className =? "stalonetray" --> doIgnore
        , className =? ".obs-wrapped" --> doFloat
        , className =? "Peek" --> doFloat
        , className =? "discord" --> doFloat
        , className =? "Gimp" --> doFloat
        , className =? "Keybase" --> doFloat
        , className =? "Calculator" --> doFloat
        , className =? "QjackCtl" --> doFloat
        , className =? "game" --> ManageHook.doFloat
        ]

-------------------------------
-- layouts
-------------------------------

tabbedConf :: Tabbed.Theme
tabbedConf =
    def
        { Tabbed.activeColor = blue
        , Tabbed.activeBorderColor = blue
        , Tabbed.inactiveColor = bg1
        , Tabbed.inactiveBorderColor = bg1
        , Tabbed.activeTextColor = black
        }

myLayoutHook =
    Docks.avoidStruts $
        smartBorders $
            workspaceDir "/home/marek" $
                windowNavigation $
                    tall ||| full
  where
    space = smartSpacing 5
    tall =
        Tabbed.addTabs shrinkText tabbedConf $
            space $
                subLayout [] Simplest $
                    boringWindows $
                        ResizableTall 1 (3 / 100) (1 / 2) []

    full =
        noBorders $ Tabbed.addTabs shrinkText tabbedConf $ boringAuto $ F.fullscreenFull $ Simplest

-- wide =
--     boringAuto $
--         space $
--             Mirror $ ResizableTall 1 (2 / 100) (5 / 6) []

-- threeCol =
--     Tabbed.addTabs shrinkText tabbedConf $
--         space $
--             subLayout [] Simplest $
--                 boringWindows $
--                     ThreeColMid 1 (3 / 100) (2 / 3)
