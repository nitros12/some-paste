{-# LANGUAGE OverloadedStrings #-}

module Highlight ( highlightPaste
                 , getStyleCss
                 , includeStyle
                 ) where

import           Data.Either                 (fromRight)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Data.Text                   as T
import           NordSyntax                  (nord)
import           Skylighting
import           Text.Blaze.Html5            hiding (map)
import           Text.Blaze.Html5.Attributes (href, rel, type_)

-- |Split text into a list of sourcelines
buildLines :: Text -> [SourceLine]
buildLines = Prelude.map (\l -> [(VerbatimStringTok, l)]) . T.lines

getStyle :: Text -> Style
getStyle s = case s of
  "espresso"    ->  espresso
  "kate"        ->  kate
  "tango"       ->  tango
  "haddock"     ->  haddock
  "monochrome"  ->  monochrome
  "breeze-dark" ->  breezeDark
  "pygments"    ->  pygments
  _             ->  nord

tokenizePaste :: Text -> Syntax -> [SourceLine]
tokenizePaste paste syntax = fromRight (buildLines paste) (tokenize config syntax paste)
  where
    config = TokenizerConfig { syntaxMap = defaultSyntaxMap
                             , traceOutput = False
                             }

includeStyle :: Text -> Html
includeStyle s = link ! href url ! rel "stylesheet"
    where url = toValue $ "/theme/" <> s

-- |A helper function to highlight a paste
highlightPaste :: Text -> Text -> Text -> Html
highlightPaste paste syntax theme = do
  let lines' = fromMaybe
              (buildLines paste)
              (tokenizePaste paste <$> lookupSyntax syntax defaultSyntaxMap)

  formatHtmlBlock opts lines'
  -- style ! type_ "text/css" $ toHtml $ styleToCss (getStyle theme)

  where
    opts = defaultFormatOpts { numberLines = True
                             , lineAnchors = True
                             }

getStyleCss :: Text -> Html
getStyleCss s = do
  let s' = getStyle s
  toHtml $ styleToCss (s')
