module Basic exposing (main)

import Html
import Parser
import Yaml.Internal.Ast as Ast


main : Html.Html msg
main =
    Ast.view test


test : Result Parser.Error Ast.Ast
test =
    Ast.build test1


test3 : String
test3 =
    """--- trash
    id:
        bioguide: B000944
        thomas: 00136
        lis: S307


    """


test1 : String
test1 =
    """--- trash
    { hey: hey, ok: wow, abc def, ghi j[klm ] }


    """


test2 : String
test2 =
    """--- trash

- aaa
-
    - bbb
- ccc
-
    - eee
    - fff
    -

         - hhh
         - iii
- ddd


"""
