module Main where

import Bot
import Data.Text
import System.Environment

main = getEnv "TOKEN" >>= runBot . pack
