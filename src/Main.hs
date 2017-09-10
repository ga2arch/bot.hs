module Main where

import           Bot
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main = TIO.readFile "token" >>= return . head . T.lines  >>= runBot
