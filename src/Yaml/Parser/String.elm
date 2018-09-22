module Yaml.Parser.String exposing (exceptions)


import Parser as P exposing ((|=), (|.))
import Yaml.Parser.Util as U
import Yaml.Parser.Ast as Ast



{-| -}
exceptions : P.Parser Ast.Value
exceptions =
    let dashed s = "---" ++ s in
    P.oneOf
      [ P.succeed Ast.Null_ -- TODO
          |. P.end
      , P.succeed (Ast.String_ << dashed)
          |. U.threeDashes
          |= U.remaining
      , P.succeed Ast.Null_
          |. U.threeDots
          |. U.remaining
      ]
      

