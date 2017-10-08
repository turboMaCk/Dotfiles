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
import XMonad
import XMonad.Hooks.ManageHelpers         (isFullscreen, doFullFloat)
import XMonad.Hooks.DynamicLog            (PP, ppVisible, ppCurrent, ppTitle, ppLayout, ppUrgent, statusBar, xmobarColor, xmobarPP, wrap)
import XMonad.Layout.NoBorders            (smartBorders)
import XMonad.Layout.Fullscreen           (fullscreenFull)
import XMonad.Layout.GridVariants
import XMonad.Layout.Spacing              (spacingWithEdge)
import qualified XMonad.Hooks.ManageDocks as Docks
import qualified XMonad.StackSet          as W

import System.Exit                        (ExitCode(ExitSuccess), exitWith)
import Data.Monoid                        (Endo)
import qualified Data.Map                 as M

-------------------------------------
-- Main
-------------------------------------

main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig


-------------------------------------
-- Config
-------------------------------------


-- Command to launch the bar.
myBar :: String
myBar = "xmobar"


-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP :: PP
myPP = xmobarPP { ppVisible = xmobarColor "#ffffff" ""
                , ppCurrent = xmobarColor "#222222" "#2E9AFE"
                , ppTitle   = xmobarColor "#2E9AFE" ""
                , ppLayout  = xmobarColor "#888888" ""
                , ppUrgent  = xmobarColor "#900000" "" . wrap "[" "]"
                }


-- Key binding to toggle the gap for the bar.
toggleStrutsKey :: XConfig a -> ( KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modM } = ( modM, xK_b )


-- Main configuration, override the defaults to your liking.
myConfig = def { modMask            = mod1Mask
               , terminal           = "urxvt"
               , workspaces         = myWorkspaces
               , keys               = myKeys
               , layoutHook         = smartBorders $ myLayoutHook
               , focusedBorderColor = "#2E9AFE"
               , normalBorderColor  = "#000000"
               , manageHook         = myManageHook <+> manageHook def
               , borderWidth        = 2
               , startupHook        = myStartupHook
               }


xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]


webW = "\xf268"
codeW = "\xf121"
communicateW = "\xf086"
emailW = "\xf0e0"
terminalW = "\xf120"
musicW = "\xf1bc"


myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $
  [ webW
  , codeW
  , communicateW
  , emailW
  , musicW
  , terminalW
  , terminalW
  , terminalW
  , terminalW ]
  where
    clickable l = [ " <action=xdotool key alt+" ++ show n ++ ">" ++ ws ++ "</action> " |
                    (i , ws) <- zip [1..9] l,
                    let n = i ]


-------------------------------------
-- Keys
-------------------------------------


myKeys conf@(XConfig { XMonad.modMask = modMasq }) = M.fromList $

    -- launch a terminal
    [ ((mod1Mask .|. shiftMask, xK_t), spawn "urxvt")

    -- launch a browser
    , ((mod1Mask .|. shiftMask, xK_b    ), spawn "chromium")

    -- launch emacs client frame
    , ((mod1Mask .|. shiftMask, xK_o    ), spawn "emacsclient -n -c")

    -- launch dmenu
    , ((modMasq,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- close focused window
    , ((modMasq,               xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMasq,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMasq .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMasq,               xK_n     ), refresh)

    -- Move focus to the next window
    -- , ((modMasq,               xK_Tab   ), windows $ W.swapMaster . W.focusDown . W.focusMaster)
    , ((modMasq,               xK_Tab     ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMasq,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMasq,               xK_k     ), windows W.focusUp)

    -- Move focus to the master window
    , ((modMasq,               xK_m     ), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modMasq,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMasq .|. shiftMask, xK_j     ), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modMasq .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink the master area
    , ((modMasq,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMasq,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMasq,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMasq              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMasq              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modMasq .|. shiftMask, xK_c     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMasq .|. shiftMask, xK_q     ), spawn "xmonad --recompile; ~/.xmonad/kill.sh; notify-send \"XMonad\" \"Reloaded!\"; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMasq, k)
     , windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMasq, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



-------------------------------
-- spawn processes
-------------------------------


myStartupHook :: X ()
myStartupHook = do
  spawn "$HOME/.xmonad/startup.sh"

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "stalonetray"  --> doIgnore
    , className =? "Spotify"      --> doFloat
    , className =? "Slack"        --> doShift communicateW
    , className =? "Thunderbird"  --> doShift emailW
    , className =? "Caprine"      --> doFloat
    , className =? "pinentry"     --> doFloat
    , Docks.manageDocks
    , isFullscreen                --> (doF W.focusDown <+> doFullFloat)
    ]


-------------------------------
-- layouts
-------------------------------

myLayoutHook = Docks.avoidStruts $ withSpaces tall ||| fullscreenFull Full ||| withSpaces half
  where
    tall = Tall 1 (3/100) (2/3)
    half = Tall 1 (3/100) (1/2)
    withSpaces layout =
      spacingWithEdge 5 $ layout

-- Local Variables:
-- flycheck-ghc-args: ("-Wno-missing-signatures")
-- End:
