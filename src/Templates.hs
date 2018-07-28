{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates ( viewPaste
                 , frontPage
                 , plainPaste
                 , aboutPage
                 ) where

import           BasicPrelude                hiding (Text)
import           Clay                        (background, height, margin, nil,
                                              none, outline, pct, transparent,
                                              width, ( # ), (-:), (?))
import qualified Clay                        as C
import           Data.Map                    (keys)
import           Data.Monoid                 ((<>))
import           Data.String                 (fromString)
import qualified Data.Text                   as ST
import           Data.Text.Lazy              (Text, toStrict, unpack)
import qualified Data.Text.Lazy              as T
import           Db                          (Paste)
import           Highlight
import           Skylighting.Syntax          (defaultSyntaxMap)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

tshow' = T.fromStrict . tshow

viewPaste :: Paste -> Text -> H.Html
viewPaste (_, _, paste, key, lang) theme = H.html $ do
  H.head $ do
    H.title . H.lazyText $ "Paste: {}: " <> tshow' key
    H.style H.! A.type_ "text/css" $ pasteCss
    includeStyle $ toStrict theme
    openGraph "og:type" "website"
    openGraph "og:site_name" "somepaste"
    openGraph "og:title" $ fromString . unpack $ T.take 120 paste
    openGraph "og:description" "A bad paste service, written in haskell."
  H.body $ highlightPaste (toStrict paste) (toStrict lang) $ toStrict theme


makeOption :: ST.Text -> H.Html
makeOption v = H.option H.! A.value (fromString . ST.unpack $ v) $ H.toHtml v

openGraph :: H.AttributeValue -> H.AttributeValue -> H.Html
openGraph p c = H.meta H.! H.customAttribute "property" p H.! A.content c

frontPage :: H.Html
frontPage = do
  H.head $ do
    H.title "SomePaste"
    H.style H.! A.type_ "text/css" $ frontCss
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
          mconcat $ map makeOption (keys defaultSyntaxMap)
      H.input
        H.! A.type_ "submit"
        H.! A.value "Paste It!"
      H.a
        H.! A.href "/about"
        H.! A.class_ "about-button" $
        "about"

renderCssMin = C.renderWith C.compact []

frontCss :: H.Html
frontCss = do
  H.toHtml . renderCssMin $ do
    C.body ? C.backgroundColor "#2E3440"
    C.a # ".about-button" ? do
      C.float C.floatRight
      C.color "#ECEFF4"
    C.textarea # ".code-input" ? do
      width (pct 100)
      height (pct 80)
      outline none none none
      C.color "#ECEFF4"
      background transparent
      "resize" -: "none"


pasteCss :: H.Html
pasteCss = do
  H.toHtml . renderCssMin $ do
    C.body ? margin nil nil nil nil
    C.pre # ".sourceCode" ? -- do
      -- height (pct 100)
      margin 0 0 0 (C.em 3)
    C.a # ".sourceLine" ? C.textDecoration C.none
    C.td # ".lineNumbers" ? C.width (C.em 1)
    C.body ? C.backgroundColor "#2E3440"


aboutPage :: H.Html
aboutPage = viewPaste (undefined, undefined, T.fromStrict aboutText, "", "plain") "nord"
  where aboutText = unlines ["SomePaste is a paste service written by Nitros [https://github.com/nitros12]"
                            ,"The site is written entirely in Haskell, using Postgres as the data store."
                            ,"My main focus when building this was speed, since other paste services are very slow"
                            ,"This site uses no JavaScript, the entire page is sent in one request."
                            ,"I average about 8ms per request with the site being 5kb in size."]


plainPaste :: Paste -> Text
plainPaste (_, _, t, _, _) = t
