module Prompt.YubiOath
  ( yubiOathPrompt
  ) where

import qualified Control.Exception.Extensible as E
import           System.IO
import           System.Process
import           XMonad
import           XMonad.Prompt

data YubiOath = YubiOath

instance XPrompt YubiOath where
  showXPrompt YubiOath = "Yubi key: "

yubiOathPrompt :: XPConfig -> X ()
yubiOathPrompt xpconfig = do
  cmds <- io listOathCodes
  mkXPrompt YubiOath xpconfig (mkComplFunFromList' cmds) typeOathCode

listOathCodes :: IO [String]
listOathCodes = uniqSort . lines <$> run "ykman oath list"

typeOathCode :: String -> X ()
typeOathCode label = do
  code <- liftIO $ run $ "ykman oath code " ++ label ++ " | awk '{print $2}'"
  spawn $ "xdotool type --clearmodifiers " ++ code

run :: String -> IO String
run cmd = getCommandOutput cmd `E.catch` \E.SomeException{} -> return ""

-- From https://github.com/xmonad/xmonad-contrib/blob/master/XMonad/Prompt/Man.hs
getCommandOutput :: String -> IO String
getCommandOutput s = do
  (pin, pout, perr, _) <- runInteractiveCommand s
  hClose pin
  output <- hGetContents pout
  _ <- E.evaluate (length output)
  hClose perr
  return output
