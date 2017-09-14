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

import           Clay
import qualified Clay                        as C

import           Skylighting.Syntax          (defaultSyntaxMap)

import           Db                          (Paste)
import           Highlight

import           Data.String                 (IsString, fromString)

import           Data.Map                    (keys)

viewPaste :: Paste -> Maybe Text -> Html
viewPaste (_, time, paste, key, lang) theme = H.html $ do
  H.head . H.title . lazyText $ format "Paste: {}: {}" [key, lang]
  H.body $ do
    highlightPaste (toStrict paste) (toStrict lang) $ fmap toStrict theme
    H.style H.! A.type_ "text/css" $ toHtml . C.render $ do
      C.body ? margin nil nil nil nil
      C.table # ".sourceCode" ? height (pct 100)
      C.td # ".lineNumbers" ? C.width (C.em 1)

makeOption :: ST.Text -> Html
makeOption v = H.option H.! A.value (fromString . ST.unpack $ v) $ H.toHtml v

frontPage :: Html
frontPage = do
  H.head . H.title $ "SomePaste"
  H.body $ do
    H.form H.! A.action "/paste" H.! A.method "post" $ do
      H.textarea ""
        H.! A.name "text"
        H.! A.class_ "code-input"
        H.! A.spellcheck "false"
      H.select
        H.! A.type_ "text"
        H.! A.name "lang"
        H.! A.class_ "lang-input" $
        mconcat $ Prelude.map makeOption (keys defaultSyntaxMap)
      H.input
        H.! A.type_ "submit"
        H.! A.value "Paste It!"
    H.style H.! A.type_ "text/css" $ toHtml . C.render $
      C.textarea # ".code-input" ? do
        width (pct 100)
        height (pct 80)
        outline none none none
        background transparent
        "resize" -: "none"


plainPaste :: Paste -> Text
plainPaste (_, _, t, _, _) = t
