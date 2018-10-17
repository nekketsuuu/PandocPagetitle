module Lib
  ( doFilter
  ) where

import Data.Char (isSpace)
import Data.Map.Strict (insert)
import Data.Maybe (fromMaybe)
import Text.Pandoc.JSON

doFilter :: Pandoc -> IO Pandoc
doFilter (Pandoc meta blocks) = do
  let title = fromMaybe "" $ getH1Title blocks
  let meta' = setPageTitle title meta
  return $ Pandoc meta' blocks

getH1Title :: [Block] -> Maybe String
getH1Title [] = Nothing
getH1Title (block:blocks) =
  case getH1Title' block of
    Nothing -> getH1Title blocks
    js -> js

getH1Title' :: Block -> Maybe String
getH1Title' (Header 1 _ inlines) =
  Just $ stringify inlines
getH1Title' (Div _ []) = Nothing
getH1Title' (Div _ (block:blocks)) =
  case getH1Title' block of
    Nothing -> getH1Title blocks
    js -> js
getH1Title' _ = Nothing

-- TODO(nekketsuuu): ad-hoc
stringify :: [Inline] -> String
stringify ss = compressSpaces $ strip $ unwords $ map stringify' ss

stringify' :: Inline -> String
stringify' (Str s)          = s
stringify' (Emph ss)        = stringify ss
stringify' (Strong ss)      = stringify ss
stringify' (Strikeout ss)   = stringify ss
stringify' (Superscript ss) = stringify ss
stringify' (Subscript ss)   = stringify ss
stringify' (SmallCaps ss)   = stringify ss
stringify' (Quoted _ ss)    = stringify ss
stringify' (Cite _ ss)      = stringify ss
stringify' (Code _ s)       = s
stringify' (Space)          = ""
stringify' (SoftBreak)      = ""
stringify' (LineBreak)      = ""
stringify' (Math _ s)       = s
stringify' (RawInline _ s)  = s
stringify' (Link _ ss _)    = stringify ss
stringify' (Image _ _ _)    = ""
stringify' (Note _)         = ""
stringify' (Span _ ss)      = stringify ss

strip :: String -> String
strip s = stripEnd $ stripStart s

stripStart :: String -> String
stripStart (c:cs) | isSpace c = stripStart cs
stripStart s = s

stripEnd :: String -> String
stripEnd s = reverse $ stripStart $ reverse s

compressSpaces :: String -> String
compressSpaces "" = ""
compressSpaces (c:cs) =
  if isSpace c then
    c : (compressSpaces $ stripStart cs)
  else
    c : (compressSpaces cs)

setPageTitle :: String -> Meta -> Meta
setPageTitle title meta =
  Meta $ insert "pagetitle" (MetaString title) map
  where map = unMeta meta 
  
