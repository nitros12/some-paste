{-# LANGUAGE OverloadedStrings #-}

module Highlight ( highlightPaste
                 ) where

import           Data.Text       (Text)
import           Data.Text       as T

import           Skylighting

import           Text.Blaze.Html

-- |A helper function to highlight a paste
highlightPaste :: Text -> Text -> IO Html
highlightPaste code syntax = do
  syntax' <- case lookupSyntax syntax defaultSyntaxMap of
    Just s  -> return s
    Nothing -> fail . T.unpack $ T.append "no available syntax for: " syntax
    -- TODO: if this fails, instead just format into lines
  sourceLines <- case tokenize config syntax' code of
    Left e  -> fail e
    Right v -> return v
  return $ formatHtmlInline opts sourceLines
  where
    opts = defaultFormatOpts { numberLines = True
                             , lineAnchors = True
                             }
    config = TokenizerConfig { syntaxMap = defaultSyntaxMap
                             , traceOutput = False
                             }


