import Control.Monad (liftM2)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sortBy, elemIndex)
import Data.Maybe
import Data.Monoid
import Data.Ord ( comparing )
import Data.Ratio
import Foreign.C.Types
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.NoBorders
import XMonad.Actions.UpdatePointer -- for updatePointer logHook
import XMonad.Actions.WindowBringer -- for gotoMenu
import XMonad.Actions.WindowGo      -- for runOrRaise
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.Place
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BinarySpacePartition hiding (Swap)
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run              -- for spawnPipe and hPutStrLn
import XMonad.Util.WindowProperties (getProp32)

-------------------------------------------------------------------------------
-- Urgency Hook: run notify-send
-------------------------------------------------------------------------------

myurgencyconf :: UrgencyConfig
myurgencyconf = UrgencyConfig { suppressWhen = XMonad.Hooks.UrgencyHook.Never, remindWhen = Dont }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

-------------------------------------------------------------------------------
-- Create virtual "screen" that doesn't actually correspond to VGA or DVI
-- It can be viewed with VNC on another machine
-- See the script my.vnc-screen
-------------------------------------------------------------------------------

rescreen_extra :: X ()
rescreen_extra = do
    -- io .  fmap nubScreens . getScreenInfo
    -- realxinesc <- withDisplay $ io . getScreenInfo
    realxinesc <- withDisplay getCleanedScreenInfo

    -- FIXME Graphics.X11.Xinerama getScreenInfo seems to return the wrong
    -- thing on my iMac at home (built in display only running fglrx
    -- proprietary driver).  Instead of returning the size of the one monitor
    -- it returns the size of the framebuffer "screen."  Normally with one
    -- monitor that would be the same but I'm extending it with
    -- "xrandr --fb WxH" to make room for the VNC virtual screen.  Then XMonad
    -- thinks the real screen is bigger than it really is.
    --
    -- Maybe I should see what this returns:
    -- http://hackage.haskell.org/packages/archive/X11/1.6.1.1/doc/html/Graphics-X11-Xrandr.html
    --
    -- Graphics.X11.Xlib.openDisplay "" >>= Graphics.X11.Xinerama.getScreenInfo

    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let xinesc   = realxinesc ++ [ Rectangle 0 1950 800 600 ]
            (xs, ys) = splitAt (length xinesc) $ map W.workspace (v:vs) ++ hs
            (a:as)   = zipWith3 W.Screen xs [0..] $ map SD xinesc
        in  ws { W.current = a
               , W.visible = as
               , W.hidden  = ys }

---------------------------------------------------------------------------------
---- Set a property on root window that shows which desktop is on each monitor
---- This will allow setting per-workspace wallpapers.
---- Possibly with a patched version of wallpaperd
---------------------------------------------------------------------------------

--dynamicLogXineramaProperty :: X ()
--dynamicLogXineramaProperty = do
--    spaces <- asks (workspaces . config)
--    withWindowSet $ setCurrentDesktops . (leifWindowSetXinerama spaces)

---- | Write a string to a property on the root window.  This property is of
---- type UTF8_STRING. The string must have been processed by encodeString
---- (dynamicLogString does this).
--setCurrentDesktops :: [ CChar ] -> X ()
--setCurrentDesktops desktops = do
--    d <- asks display
--    r <- asks theRoot
--    xlog <- getAtom "_NET_CURRENT_DESKTOPS_XINERAMA"
--    ustring <- getAtom "CARDINAL"
--    io $ changeProperty8 d r xlog ustring propModeReplace desktops

--tagToIndex :: [String] -> String -> Maybe Int
--tagToIndex wrkspaces tag = do
--    elemIndex tag wrkspaces

--leifWindowSetXinerama :: [String] -> WindowSet -> [CChar]
--leifWindowSetXinerama wrkspaces ws = onscreen
--    where onscreen  = map fromIntegral $ catMaybes $ map (tagToIndex wrkspaces) $ map (W.tag . W.workspace)
--                            . sortBy (comparing W.screen) $ W.current ws : W.visible ws

-------------------------------------------------------------------------------
-- ManageHook
-------------------------------------------------------------------------------

myWorkspaces :: [[Char]]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

-------------------------------------------------------------------------------
-- ManageHook
-------------------------------------------------------------------------------

myManageHook :: Query (Endo WindowSet)
myManageHook = let windowAction a w = withDisplay $ \d -> io $ a d w
                   windowActionHook a = ask >>= \w -> liftX (a w) >> idHook
    in
    -- placeHook should be run last, which means it should be first
    -- placeHook is causing problems with sxiv windows that want to place
    -- themselves via the script imgprev.sh I wrote.  So I took this out
    -- until I can figure out how to apply this hook conditionally.
    -- placeHook ( withGaps (16,0,16,0) (smart (0.5,0.5)) )
    -- <+>
    composeAll [
    resource  =? "xterm1"           --> doShift "1"
  , resource  =? "xterm2"           --> doShift "2"
  , resource  =? "xterm3"           --> doShift "3"
  , resource  =? "xterm4"           --> doShift "4"
  , resource  =? "xterm5"           --> doShift "5"
  , resource  =? "xterm6"           --> doShift "6"
  , resource  =? "xterm7"           --> doShift "7"
  , resource  =? "xterm8"           --> doShift "8"
  , resource  =? "xterm9"           --> doShift "9"
  , className =? "stalonetray"      --> doShift "9"
  , className =? "Google-chrome"    --> doShiftAndGo "5"
  , className =? "Gvim"             --> doShiftAndGo "3"
  , className =? "Sxiv"             --> doFloat
  , className =? "Dzen2"            --> doIgnore
  , title     =? "Slack Call Minipanel" --> doShift "9"
  , className =? "Zenity"           --> doFloat
  , title     =? "Wine System Tray" --> doShift "9"
  , title     =? "plugin-container" --> doFullFloat
  , isFullscreen                    --> doFullFloat
  , title     =? "LeifWallpaper"    -->
        ( doIgnore `mappend` windowActionHook (windowAction lowerWindow) )
  ] <+> manageHook defaultConfig
  where
  doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift

-------------------------------------------------------------------------------
-- Layout
-------------------------------------------------------------------------------

-- import qualified XMonad.Layout.HintedTile as HT
-- myLayout = spacing 20 (avoidStruts $ lessBorders Screen $ reflectHoriz $ XMonad.Tall nmaster delta ratio)
--        ||| spacing 20 (avoidStruts $ lessBorders Screen $ reflectHoriz $ hintedTile HT.Tall)
--        ||| spacing 20 (avoidStruts $ lessBorders Screen $ Full)
--        ||| spacing 20 Grid

--       ||| smartSpacing 10 (avoidStruts emptyBSP)

myLayout = smartBorders $ layoutHintsWithPlacement (0.5, 0.5) (smartSpacing 10 (avoidStruts $ XMonad.Tall nmaster delta ratio))
       ||| noBorders Full
       ||| layoutHintsWithPlacement (0.5, 0.5) (smartSpacing 10 (SplitGrid XMonad.Layout.GridVariants.R 1 1 (1/2) (16/10) (3/100)))
  where
      nmaster    = 1
      delta      = 3/100
      ratio      = 1/2

-------------------------------------------------------------------------------
-- Control compositing
-------------------------------------------------------------------------------

setInverted :: Window -> X ()
setInverted w = withDisplay $ \dpy -> do
    a <- getAtom "_COMPTON_INVERT"
    c <- getAtom "CARDINAL"
    current  <- fromMaybe [1] `fmap` getProp32 a w
    let desired = [ ( head current + 1 ) `rem` 2 ]
    io $ changeProperty32 dpy w a c propModeReplace desired

-- | Sets the opacity of inactive windows to the specified amount
-- | Also workspace 6 is never transparent.
-- | And mplayer is never transparent.
leifFadeInactiveLogHook :: Rational -> X ()
leifFadeInactiveLogHook = fadeOutLogHook . fadeIf leifIsUnfocused
leifIsUnfocused = do
  w <- ask
  ws <- liftX $ gets windowset
  className <- ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resClass $ io $ getClassHint d w)
  let fadedWS = case W.findTag w ws of
                    Just i -> i /= "6" -- Workspace "6" isn't a fading WS
                    Nothing -> False
      unfocused = maybe True (w /=) $ W.peek ws
      mplayer   = (className == "MPlayer") -- mplayer2 for mplayer2
  return $ fadedWS && unfocused && not mplayer

-------------------------------------------------------------------------------
-- Key Bindings
-------------------------------------------------------------------------------

myKeys c = mkKeymap c $
    [ ("M-p",                    spawn "my.dmenu_run")
    -- , ("M-t",                    withAll hide) -- %! Test making compton fade in and out a window
    , ("M-j",                    windows W.focusDown)
    , ("M-k",                    windows W.focusUp  )
    , ("M-m",                    windows W.focusMaster  )
    , ("M-<Return>",             windows W.swapMaster)
    , ("M-S-j",                  windows W.swapDown  )
    , ("M-S-k",                  windows W.swapUp    )
    , ("M-h",                    sendMessage Shrink)
    , ("M-l",                    sendMessage Expand)
    , ("M-,",                    sendMessage (IncMasterN 1))
    , ("M-.",                    sendMessage (IncMasterN (-1)))
    , ("<XF86AudioMute>",        spawn "my.volume togglemute")
    , ("<XF86AudioLowerVolume>", spawn "my.volume down")
    , ("<XF86AudioRaiseVolume>", spawn "my.volume up")
    , ("<XF86MonBrightnessDown>",spawn "my.xbacklight -dec 9")
    , ("<XF86MonBrightnessUp>",  spawn "my.xbacklight -inc 9")
    , ("M-S-<F6>",               spawn "my.xbacklight -dec 9")
    , ("M-S-<F7>",               spawn "my.xbacklight -inc 9")
    , ("M-S-<F8>",               spawn "my.volume togglemute")
    , ("M-S-<F9>",               spawn "my.volume down")
    , ("M-S-<F10>",              spawn "my.volume up")
    , ("<XF86Eject>",            spawn "devmon --unmount-removable")
    , ("M-S-<Space>",            sendMessage NextLayout)

    , ("M-<Space> M-S-<Space>",  setLayout $ XMonad.layoutHook c)
    , ("M-<Space> a",            spawn "xdotool type blah")
    , ("M-<Space> <Return>",     spawn "xdotool getactivewindow mousemove --window %1 --polar 100 250 click --clearmodifiers 1")
    , ("M-<Space> <Space>",      spawn "my.terminal")
    , ("M-<Space> M-<Space>",    spawn "my.terminal")
    , ("M-<Space> S-c",          kill)
    , ("M-<Space> ;",            spawn "my.lockscreen"                                    )
    , ("M-<Space> b",            sendMessage ToggleStruts                                 )
    , ("M-<Space> c",            spawn "xclip -o | xclip -selection clipboard"            )
    , ("M-<Space> d",            withFocused toggleBorder                                 )
    , ("M-<Space> e",            spawn "xdotool key F13; sleep 1; xdotool key F14"        )
    , ("M-<Space> g",            gotoMenuArgs [ "-l", "5" ]                               )
    , ("M-<Space> S-g",          bringMenu                                                )
    -- Check out XMonad.Actions.GroupNavigation instead of WindowGo / raiseNextMaybe
    , ("M-<Space> o",            raiseNextMaybe (spawn "my.terminal my.ranger"            ) ( fmap ( "ranger"  `isPrefixOf` ) title )  )
    , ("M-<Space> i",            withFocused setInverted)
    , ("M-<Space> v",            raiseNextMaybe (spawn "my.terminal vim"                  ) ( fmap ( "VIM"     `isSuffixOf` ) title )  )
    , ("M-<Space> '",            raiseNextMaybe (spawn "my.terminal"                      ) ( fmap ( "bash"    `isPrefixOf` ) title )  )
    , ("M-<Space> s",            raiseNextMaybe (spawn "my.dmenu_run"                     ) ( fmap ( "ssh"     `isPrefixOf` ) title )  )
    , ("M-<Space> t",            withFocused $ windows . W.sink)
    , ("M-<Space> S-t",          withFocused $ windows . (flip W.float $ W.RationalRect 0 ((-1) % 12) 1 (13 % 12)))
    , ("M-<Space> M-t",          withFocused $ windows . (flip W.float $ W.RationalRect 0 ((-1) % 6) 1 (7 % 6)))
    , ("M-<Space> =",            rescreen_extra)
    , ("M-<Space> <Backspace>",  rescreen)
    , ("M-<Space> q",            spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    ]
    ++
    [ ("M-C-l", sendMessage $ ExpandTowards BSP.R)
    , ("M-C-h", sendMessage $ ExpandTowards BSP.L)
    , ("M-C-j", sendMessage $ ExpandTowards BSP.D)
    , ("M-C-k", sendMessage $ ExpandTowards BSP.U)
    , ("M-C-y", sendMessage $ ShrinkFrom BSP.R)
    , ("M-C-u", sendMessage $ ShrinkFrom BSP.L)
    , ("M-C-i", sendMessage $ ShrinkFrom BSP.D)
    , ("M-C-o", sendMessage $ ShrinkFrom BSP.U)
    , ("M-s",   sendMessage $ BSP.Swap)
    , ("M-C-s", sendMessage $ Rotate) ]
    ++
    [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.greedyView)
                                      , ("S-", windows . W.shift)]
    ]
    ++
    [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
         | (key, scr)  <- zip "wer" [0,1,2]
         , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    ]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO()
main = do
    h <- spawnPipe "my.xmonad-timetracker"
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ withUrgencyHookC LibNotifyUrgencyHook myurgencyconf $
        ewmh $ docks $ defaultConfig {
    modMask = mod4Mask
  , terminal           = "my.terminal"
  , workspaces         = myWorkspaces
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , normalBorderColor  = "#555555"
  , focusedBorderColor = "#FFA500"
  , borderWidth        = 5
  , layoutHook         = myLayout
  , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook
  , manageHook         = myManageHook
  , logHook            =
    (updatePointer (0.5, 0.5) (0.99, 0.99)) -- This allows keynav to work multihead
    -- from the comment in keynav.c /* Figure which display the cursor is on */
    <+>
    leifFadeInactiveLogHook 0.6
    <+>
    -- dynamicLogXineramaProperty
    -- <+>
    ( dynamicLogWithPP $ xmobarPP  {   ppOutput = hPutStrLn xmproc
                                     , ppTitle  = shorten 1024
                                   }
    )
    <+>
    ( dynamicLogWithPP $ defaultPP {   ppOutput = hPutStrLn h
                                     , ppTitle  = shorten 1024
                                     , ppOrder = \(_:_:t:_) -> [t] }
    )
    <+>
    logHook defaultConfig
  , keys               = myKeys
}

