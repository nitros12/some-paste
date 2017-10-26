module NordSyntax (nord) where

import           Skylighting.Types


nord0 :: Maybe Color
nord0  = toColor "#2E3440"

nord1 :: Maybe Color
nord1  = toColor "#3B4252"

nord2 :: Maybe Color
nord2  = toColor "#434C5E"

nord3 :: Maybe Color
nord3  = toColor "#4C566A"

nord4 :: Maybe Color
nord4  = toColor "#D8DEE9"

nord5 :: Maybe Color
nord5  = toColor "#E5E9F0"

nord6 :: Maybe Color
nord6  = toColor "#ECEFF4"

nord7 :: Maybe Color
nord7  = toColor "#8FBCBB"

nord8 :: Maybe Color
nord8  = toColor "#88C0D0"

nord9 :: Maybe Color
nord9  = toColor "#81A1C1"

nord10 :: Maybe Color
nord10 = toColor "#5E81AC"

nord11 :: Maybe Color
nord11 = toColor "#BF616A"

nord12 :: Maybe Color
nord12 = toColor "#D08770"

nord13 :: Maybe Color
nord13 = toColor "#EBCB8B"

nord14 :: Maybe Color
nord14 = toColor "#A3BE8C"

nord15 :: Maybe Color
nord15 = toColor "#B48EAD"

colours :: [(TokenType, TokenStyle)]
colours = [(KeywordTok, TokenStyle nord9 Nothing False False False)
          ,(DataTypeTok, TokenStyle nord15 Nothing False False False)
          ,(DecValTok, TokenStyle nord15 Nothing False False False)
          ,(BaseNTok, TokenStyle nord15 Nothing False False False)
          ,(FloatTok, TokenStyle nord15 Nothing False False False)
          ,(ConstantTok, TokenStyle nord4 Nothing True False False)
          ,(CharTok, TokenStyle nord14 Nothing False False False)
          ,(SpecialCharTok, TokenStyle nord14 Nothing False False False)
          ,(StringTok, TokenStyle nord14 Nothing False False False)
          ,(VerbatimStringTok, TokenStyle nord13 Nothing False False False)
          ,(SpecialStringTok, TokenStyle nord13 Nothing False False False)
          ,(ImportTok, TokenStyle nord9 Nothing True False False)
          ,(CommentTok, TokenStyle nord4 Nothing False False False)
          ,(DocumentationTok, TokenStyle nord12 Nothing False False False)
          ,(AnnotationTok, TokenStyle nord12 Nothing False False False)
          ,(CommentVarTok, TokenStyle nord4 Nothing False False False)
          ,(OtherTok, TokenStyle nord4 Nothing False False False)
          ,(FunctionTok, TokenStyle nord8 Nothing False False False)
          ,(VariableTok, TokenStyle nord4 Nothing False False False)
          ,(ControlFlowTok, TokenStyle nord9 Nothing True False False)
          ,(OperatorTok, TokenStyle nord9 Nothing False False False)
          ,(BuiltInTok, TokenStyle nord9 Nothing False False False)
          ,(PreprocessorTok, TokenStyle nord10 Nothing True False False)
          ,(AttributeTok, TokenStyle nord7 Nothing False False False)
          ,(NormalTok, TokenStyle nord6 Nothing False False False)]

nord :: Style
nord = Style { tokenStyles = colours
             , defaultColor = nord6
             , backgroundColor = nord3
             , lineNumberColor = nord7
             , lineNumberBackgroundColor = nord3
             }
