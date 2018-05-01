module Basic exposing (main)

import Html
import Parser
import Yaml.Internal.Ast as Ast


main : Html.Html msg
main =
    Html.code [] [ Html.text (toString test) ]


test : Result Parser.Error Ast.Ast
test =
    Ast.build
        """--- trash
        { hey: hey, ok: wow, [ abc def, ghi j[klm ] }


        """
