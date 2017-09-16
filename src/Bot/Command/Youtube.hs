{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Bot.Command.Youtube where

import           Bot.Command.Base
import           Bot.Command.Base.Types
import           Bot.Command.Types
import           Bot.Command.Youtube.Types
import           Bot.Types
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.UUID
import           Data.UUID.V4
import           System.Exit
import           System.Process.Typed

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
import qualified Text.RE.PCRE.ByteString.Lazy as R
import qualified Text.RE.Replace as R
import qualified Web.Telegram.API.Bot.Requests as TG

download :: (Functor f, MonadFree f m, Youtube :<: f) => T.Text -> m (Either T.Text FilePath)
download url = liftF . inj $ Download url id

getTitle :: (Functor f, MonadFree f m, Youtube :<: f) => T.Text -> m (Either T.Text T.Text)
getTitle url = liftF . inj $ GetTitle url id

youtubeCommand :: R.Match T.Text -> Free (Base :+: Youtube) ()
youtubeCommand match = do
  let videoId = R.captureText [R.cp|1|] match
  send "Downloading .."
  audioFilepath <- download videoId
  case audioFilepath of
    Left e -> send e
    Right path -> do
      Right title <- getTitle videoId
      uploadAudio title path

instance Eval UserMonad Youtube where
  runAlgebra (Download videoId next) = do
    out <- liftIO $ youtubeDl videoId
    next out

  runAlgebra (GetTitle videoId next) = do
    out <- liftIO $ getTitle' videoId
    next out

getTitle' videoId = do
  let ydlConfig = proc "youtube-dl" ["-e", T.unpack videoId]
  (exitCode, output, err) <- readProcess ydlConfig
  case exitCode of
    ExitSuccess -> return $ Right $ T.pack $ C.unpack output
    ExitFailure code -> return $ Left $ T.pack $ C.unpack err

youtubeDl videoId = do
  uuid <- nextRandom
  let filename = "tmp/" ++ toString uuid ++ "/%(title)s.%(ext)s"
  let ydlConfig = proc "youtube-dl" ["--audio-format",
                                     "mp3",
                                     "--no-playlist",
                                     "-x", "-o", filename,
                                     T.unpack videoId]
  (exitCode, output, err) <- readProcess ydlConfig
  case exitCode of
    ExitSuccess -> do
      regex <- R.compileRegex "\\[ffmpeg\\] Destination: (.*?)\\n"
      let match = output R.?=~ regex
      if R.matched match
        then return $ Right $ C.unpack $ R.captureText [R.cp|1|] match
        else return $ Left "error processing video"
    ExitFailure code -> return $ Left $ T.pack $ C.unpack err
