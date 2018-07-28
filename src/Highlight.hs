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
-- import           Hasmin

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
includeStyle s = style ! type_ "text/css" $ getStyleCss s

-- |A helper function to highlight a paste
highlightPaste :: Text -> Text -> Text -> Html
highlightPaste paste syntax theme = do
  let lines' = fromMaybe
              (buildLines paste)
              (tokenizePaste paste <$> lookupSyntax syntax defaultSyntaxMap)

  formatHtmlBlock opts lines'

  where
    opts = defaultFormatOpts { numberLines = True
                             , lineAnchors = True
                             }

fromRight' :: Show a => Either a b -> b
fromRight' (Left a) = error . show $ a
fromRight' (Right b) = b

getStyleCss :: Text -> Html
getStyleCss = toHtml . styleToCss . getStyle
