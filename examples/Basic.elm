module Basic exposing (main)

import Html
import Parser
import Yaml.Internal.Ast as Ast


main : Html.Html msg
main =
    Ast.view test


test : Result Parser.Error Ast.Ast
test =
    Ast.build test4


test4 : String
test4 =
    """--- trash
- aaa
-
    - {}
    - [bbb]


    """


test3 : String
test3 =
    """--- trash
aaa:
    bbb: { hey: ok }
    ddd: { hey: ok }
kkk: {}
jjj: []
iii:
  hhh: hhh
  aaa: rrr
  ccc:
    vvv: vvv
    ggg: ggg


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
