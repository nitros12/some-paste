{-# LANGUAGE OverloadedStrings #-}

module Highlight ( highlightPaste
                 ) where

import           Data.Text                   (Text)
import           Data.Text                   as T

import           Skylighting
import           Skylighting.Types

import           Text.Blaze.Html5            hiding (map)
import           Text.Blaze.Html5.Attributes (type_)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

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
  _             ->  pygments

-- |A helper function to highlight a paste
highlightPaste :: Text -> Text -> Text -> Html
highlightPaste code syntax theme = do
  let syntax' = lookupSyntax syntax defaultSyntaxMap
  case syntax' >>= (\s -> eitherToMaybe $ tokenize config s code) of
      Just v  -> formatHtmlBlock opts v >> css
      Nothing -> formatHtmlBlock opts (buildLines code) >> css
  where
    opts = defaultFormatOpts { numberLines = True
                             , lineAnchors = True
                             }
    config = TokenizerConfig { syntaxMap = defaultSyntaxMap
                             , traceOutput = False
                             }
    css = style ! type_ "text/css" $ toHtml $ styleToCss (getStyle theme)
