module Basic exposing (main)

import Html
import Parser
import Yaml.Internal.Ast as Ast


main : Html.Html msg
main =
    Html.text (toString test)


test : Result Parser.Error Ast.Ast
test =
    Ast.build
        """--- trash
        { good: [[ok]], other: {fine: hey} }


        """
