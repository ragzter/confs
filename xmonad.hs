
-- Incredibly bloated right now

import XMonad
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W
import XMonad.Prompt
import XMonad.Prompt.Window

main = xmonad $ def { modMask = mod4Mask }
                    `additionalKeysP`
                    [ ("C-t C-t", toggleWS)
                    , ("C-t c", spawn "xterm")
                    , ("C-t S-1", spawn "dmenu_run")
                    , ("C-t <Tab>", windows W.focusDown)
                    , ("C-t t", sendKey controlMask xK_t)
                    , ("<Page_Up>", sendMessage Expand)
                    , ("<Page_Down>", sendMessage Shrink)
                    , ("C-t a", windows copyToAll)
                    , ("C-t s", windows W.swapDown)
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
