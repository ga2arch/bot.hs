module Main where

import Bot
import qualified Data.Text.IO as TIO

main = TIO.readFile "token" >>= runBot
