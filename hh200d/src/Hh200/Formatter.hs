module Hh200.Formatter (formatHh200) where

import Hh200.LexHh (tokens)
import Hh200.ParHh (pScript)
import Hh200.PrintHh (printTree)
import Hh200.AbsHh (Script)
import Hh200.ErrM (Err)

formatHh200 :: String -> Either String String
formatHh200 input =
  case pScript (tokens input) of
    Left err -> Left err
    Right ast -> Right (printTree ast)
