{-# LANGUAGE OverloadedStrings #-}

module Templates ( viewPaste
                 , frontPage
                 , plainPaste
                 ) where

import           Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.Text                   as ST
import           Data.Text.Format
import           Data.Text.Lazy              (Text, fromStrict, toStrict)
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
  body $ do
    highlightPaste (toStrict paste) (toStrict lang) $ fmap toStrict theme
    style H.! A.type_ "text/css" $ toHtml . C.render $ do
      C.body ? margin nil nil nil nil
      C.table # ".sourceCode" ? height (pct 100)
      C.td # ".lineNumbers" ? C.width (C.em 1)


frontPage :: Html
frontPage = do
  H.head . H.title $ "SomePaste"
  body $ do
    form H.! A.action "/paste" H.! A.method "post" $ do
      textarea input
        H.! A.name "text"
        --H.! A.class_ "code-input"
        --H.! A.placeholder "input"
      input
        H.! A.type_ "text"
        H.! A.name "lang"
        H.! A.class_ "lang-input"
        H.! A.placeholder "language"
        H.! A.autocomplete "false"
      input
        H.! A.type_ "submit"
        H.! A.value "Submit"
    style H.! A.type_ "text/css" $ toHtml . C.render $ do
      C.input # ".code-input" ? do
        C.width (pct 100)
        C.height (pct 80)

plainPaste :: Paste -> Text
plainPaste (_, _, t, _, _) = t
