module Main where

import           Bot
import           Data.Monoid
import           System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  home <- getHomeDirectory
  let tokenPath = home <> "/.token"
  token <- TIO.readFile tokenPath
  startBot . head $ T.lines token
