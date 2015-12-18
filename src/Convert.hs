{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Convert a #nixos log from http://nixos.org/irc/logs to ZNC
-- format.
--

module Main where

import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.Monoid
import           Data.Time
import           System.Environment
import           System.Process

-- | Convert a #nixos log from http://nixos.org/irc/logs to ZNC
-- format.
main :: IO ()
main =
  do args <- getArgs
     case args of
       [] ->
         mapM_ (\d ->
                  let date = formatTime defaultTimeLocale "%Y%m%d" d
                  in catch (callCommand
                              ("wget -c http://nixos.org/irc/logs/log." ++ date))
                           (\(_ :: SomeException) ->
                              do putStrLn ("Log for date " ++
                                           date ++
                                           " doesn't exist on the server. Creating empty file ...")
                                 S.writeFile ("log." ++ date)
                                             ""))
               [fromGregorian 2012 1 2 .. fromGregorian 2015 12 18]
       fps ->
         void (mapConcurrently
                 (\fp ->
                    do text <- S.readFile fp
                       S.writeFile
                         (translatePath fp)
                         (S.unlines (filter (not . S.null)
                                            (map translateLine (S.lines text))))
                       S.putStrLn ("Translated " <> S.pack fp))
                 fps)

-- | Translate the filename to the filename format of ZNC logs.
translatePath :: FilePath -> FilePath
translatePath ('l':'o':'g':'.':date) = "#nixos_" ++ date ++ ".log"
translatePath fp = error ("Invalid file path name: " ++ fp)

-- | Translate an irc line.
translateLine :: ByteString -> ByteString
translateLine t =
  case S.splitAt 8 t of
    (time,rest) ->
      let body =
            if S.isPrefixOf "<" rest
               then rest
               else
                 case S.split ' ' rest of
                   ["Nick","change:",from,"->",to] ->
                     "*** " <> from <> " is now known as " <> to
                   ("---":_) ->
                     mempty
                   [name,"joined","#nixos."] ->
                     "*** Joins: " <> name
                   [name,ident,"got","netsplit."] ->
                     "*** Quits: " <> name <> " " <> ident <> " (netsplit)"
                   [name,ident,"got","lost","in","the","net-split."] ->
                     "*** Quits: " <> name <> " " <> ident <> " (netsplit)"
                   [name,ident,"returned","to","#nixos."] ->
                     "*** Joins: " <> name <> " " <> ident
                   [name,ident,"joined","#nixos."] ->
                     "*** Joins: " <> name <> " " <> ident
                   [name,ident,"left","#nixos."] ->
                     "*** Parts: " <> name <> " " <> ident
                   (name:ident:"left":"#nixos":reason) ->
                     "*** Parts: " <> name <> " " <> ident <> " " <> S.unwords reason
                   ("Last":"message":"repeated":_) ->
                     ""
                   (name:ident:"left":"irc:":xs) ->
                     "*** Quits: " <> name <> " " <> ident <> " (" <> S.unwords xs <> ")"
                   ("Action:":name:xs) ->
                     "* " <> name <> " " <> S.unwords xs
                   _ ->
                     ""
      in if S.null body
            then body
            else S.take 6 time <> ":00] " <> body
