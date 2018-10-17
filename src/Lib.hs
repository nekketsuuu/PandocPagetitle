module Lib
  ( doFilter
  ) where

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
  Just $ unwords $ map show inlines
getH1Title' (Div _ []) = Nothing
getH1Title' (Div _ (block:blocks)) =
  case getH1Title' block of
    Nothing -> getH1Title blocks
    js -> js
getH1Title' _ = Nothing

setPageTitle :: String -> Meta -> Meta
setPageTitle title meta =
  Meta $ insert "pagetitle" (MetaString title) map
  where map = unMeta meta 
  
