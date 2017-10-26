{-# LANGUAGE OverloadedStrings #-}

module Templates ( viewPaste
                 , frontPage
                 , plainPaste
                 ) where

import           Clay                        (background, height, margin, nil,
                                              none, outline, pct, transparent,
                                              width, ( # ), (-:), (?))
import qualified Clay                        as C
import           Data.Map                    (keys)
import           Data.String                 (fromString)
import qualified Data.Text                   as ST
import           Data.Text.Format
import           Data.Text.Lazy              (unpack, Text, toStrict)
import qualified Data.Text.Lazy as T
import           Db                          (Paste)
import           Highlight
import           Skylighting.Syntax          (defaultSyntaxMap)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

viewPaste :: Paste -> Text -> H.Html
viewPaste (_, _, paste, key, lang) theme = H.html $ do
  H.head $ do
    H.title . H.lazyText $ format "Paste: {}: " [key]
    openGraph "og:type" "website"
    openGraph "og:site_name" "somepaste"
    openGraph "og:title" $ fromString . unpack $ T.take 120 paste
    openGraph "og:description" "A bad paste service, written in haskell."
  H.body $ do
    highlightPaste (toStrict paste) (toStrict lang) $ toStrict theme
    H.style H.! A.type_ "text/css" $ H.toHtml . C.render $ do
      C.body ? margin nil nil nil nil
      C.pre # ".sourceCode" ? do
        height (pct 100)
        margin 0 0 0 (C.em 3)
      C.a # ".sourceLine" ? do
        C.textDecoration C.none
      C.td # ".lineNumbers" ? C.width (C.em 1)

makeOption :: ST.Text -> H.Html
makeOption v = H.option H.! A.value (fromString . ST.unpack $ v) $ H.toHtml v

openGraph :: H.AttributeValue -> H.AttributeValue -> H.Html
openGraph p c = H.meta H.! H.customAttribute "property" p H.! A.content c

frontPage :: H.Html
frontPage = do
  H.head $ do
    H.title "SomePaste"
    openGraph "og:type" "website"
    openGraph "og:title" "somepaste"
    openGraph "og:description" "A bad paste service, written in haskell."
  H.body $ do
    H.form H.! A.action "/paste" H.! A.method "post" $ do
      H.textarea ""
        H.! A.name "text"
        H.! A.class_ "code-input"
        H.! A.spellcheck "false"
      H.select
        H.! A.type_ "text"
        H.! A.name "lang"
        H.! A.class_ "lang-input" $ do
          makeOption "plain"
          mconcat $ Prelude.map makeOption (keys defaultSyntaxMap)
      H.input
        H.! A.type_ "submit"
        H.! A.value "Paste It!"
    H.style H.! A.type_ "text/css" $ H.toHtml . C.render $
      C.textarea # ".code-input" ? do
        width (pct 100)
        height (pct 80)
        outline none none none
        background transparent
        "resize" -: "none"


plainPaste :: Paste -> Text
plainPaste (_, _, t, _, _) = t
