module Main where

import Text.Pandoc.JSON

import Lib

main :: IO ()
main = toJSONFilter doFilter
