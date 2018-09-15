module Yaml.Parser.Null exposing (inline)

import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast


inline : P.Parser Ast.Value
inline =
  P.succeed Ast.Null_
    |. U.newLine
