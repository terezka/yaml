module Basic exposing (main)

import Html
import Parser
import Yaml.Internal.Ast as Ast


main : Html.Html msg
main =
    Ast.view test


test : Result Parser.Error Ast.Ast
test =
    Ast.build
        """--- trash
        - hey
        - { ok: rhejkt, hrekwr: f}


        """


test1 : String
test1 =
    """--- trash
    { hey: hey, ok: wow, [ abc def, ghi j[klm ] }


    """
