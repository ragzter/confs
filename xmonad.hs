
-- Incredibly bloated right now

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Layout.BinarySpacePartition

startup :: X ()
-- startup = return ()
startup = do
  spawn "xset r rate 200 30"
  spawnOn "1" "emacs"
  spawnOn "2" "google-chrome"
  spawnOn "3" "xterm"
  spawnOn "3" "xterm"

main = xmonad $ def { modMask = mod4Mask
                    , layoutHook = emptyBSP
                    , startupHook = startup
                    , manageHook = manageSpawn
                    , focusedBorderColor = "#FF00FF"
                    }
                    `additionalKeysP`
                    [ ("C-t C-t", toggleWS)
                    , ("C-t c", spawn "xterm")
                    , ("C-t e", spawn "xterm")
                    , ("C-t g", spawn "firefox")
                    , ("C-t o", spawn "dmenu_run")
                    , ("C-t <Tab>", windows W.focusDown)
                    , ("C-t t", sendKey controlMask xK_t)
                    , ("C-t s", sendMessage Swap)
                    , ("C-t r", sendMessage Rotate)
                    , ("<Page_Up>", sendMessage $ ExpandTowards U)
                    , ("<Page_Down>", sendMessage $ ExpandTowards D)
                    , ("C-<Page_Up>", sendMessage $ ExpandTowards R)
                    , ("C-<Page_Down>", sendMessage $ ExpandTowards L)
                    , ("C-t a", windows copyToAll)
                    , ("C-t k", kill1)
                    , ("C-t x", killAllOtherCopies)
                    , ("C-t 0", toggleOrView "1")
                    , ("C-t 1", toggleOrView "2")
                    , ("C-t 2", toggleOrView "3")
                    , ("C-t 3", toggleOrView "4")
                    , ("C-t 4", toggleOrView "5")
                    , ("C-t 5", toggleOrView "6")
                    , ("C-t 6", toggleOrView "7")
                    , ("C-t 7", toggleOrView "8")
                    , ("C-t 8", toggleOrView "9")
                    , ("C-t C-0", windows (W.shift "1"))
                    , ("C-t C-1", windows (W.shift "2"))
                    , ("C-t C-2", windows (W.shift "3"))
                    , ("C-t C-3", windows (W.shift "4"))
                    , ("C-t C-4", windows (W.shift "5"))
                    , ("C-t C-5", windows (W.shift "6"))
                    , ("C-t C-6", windows (W.shift "7"))
                    , ("C-t C-7", windows (W.shift "8"))
                    , ("C-t C-8", windows (W.shift "9"))
                    , ("C-t S-0", windows (copy "1"))
                    , ("C-t S-1", windows (copy "2"))
                    , ("C-t S-2", windows (copy "3"))
                    , ("C-t S-3", windows (copy "4"))
                    , ("C-t S-4", windows (copy "5"))
                    , ("C-t S-5", windows (copy "6"))
                    , ("C-t S-6", windows (copy "7"))
                    , ("C-t S-7", windows (copy "8"))
                    , ("C-t S-8", windows (copy "9"))
                    ]
