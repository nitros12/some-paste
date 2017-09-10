{-# LANGUAGE OverloadedStrings #-}

module Templates where

import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Format

import           Data.Int                    (Int64)
import           Data.Time.Clock

import           Clay                        (height, margin, nil, pct, (?), (#))
import qualified Clay                        as C

import           Db                          (Paste)
import           Highlight

viewPaste :: Paste -> Maybe Text -> Html
viewPaste (_, time, paste, key, lang) theme = html $ do
  H.head . H.title . lazyText $ format "Paste: {}: {}" [key, lang]
  body . pageBody $ highlightPaste paste lang theme


pageBody :: Html -> Html
pageBody content = do
  content
  style H.! A.type_ "text/css" $ toHtml . C.render $ do
    C.body ? margin nil nil nil nil
    C.table # ".sourceCode" ? height (pct 100)
