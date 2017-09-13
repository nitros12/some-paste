{-# LANGUAGE OverloadedStrings #-}

module Templates where

import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.Text                   as ST
import           Data.Text.Format
import           Data.Text.Lazy              (toStrict, fromStrict, Text)
import qualified Data.Text.Lazy              as T

import           Data.Int                    (Int64)
import           Data.Time.Clock

import           Clay                        (height, margin, nil, pct, ( # ),
                                              (?))
import qualified Clay                        as C

import           Db                          (Paste)
import           Highlight

viewPaste :: Paste -> Maybe Text -> Html
viewPaste (_, time, paste, key, lang) theme = html $ do
  H.head . H.title . lazyText $ format "Paste: {}: {}" [key, lang]
  body . pageBody $ highlightPaste (toStrict paste) (toStrict lang) $ fmap toStrict theme


pageBody :: Html -> Html
pageBody content = do
  content
  style H.! A.type_ "text/css" $ toHtml . C.render $ do
    C.body ? margin nil nil nil nil
    C.table # ".sourceCode" ? height (pct 100)

plainPaste :: Paste -> Text
plainPaste (_, _, t, _, _) = t
