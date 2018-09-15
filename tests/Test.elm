module Test exposing (main)

import Parser
import Yaml.Parser as Yaml
import Html


{- 

-}

main : Html.Html msg 
main =
  case Parser.run Yaml.parser randomTest of 
    Ok value -> yamlValueToHtml value
    Err error -> Html.text (String.join ", " (List.map errorToString error))


randomTest : String
randomTest =
  """
  fdsfdf
fdsfds

"""

testDocumentBegin : String
testDocumentBegin =
  """

--- trash

- id:
    "bioguide": B000944
    thomas: '00136'
    eee: eee
    govtrack: 400050
    opensecrets: N00003535
    votesmart: 27018
    fec:
    - H2OH13033
    - S6OH00163
    cspan: 5051
    wikipedia:  Sherrod Brown1
    house_history: 9996
    ballotpedia: Sherrod Brown
    maplight: 168
    icpsr: 29389
    wikidata: Q381880
    google_entity_id: kg:/m/034s80
  name:
    first: Sherrod
    last: Brown
    official_full: Sherrod Brown
  bio:
    birthday: '1952-11-09'
    gender: M
    religion: Lutheran
  terms:
  - type: rep
    start: '1993-01-05'
    end: '1995-01-03'
    state: OH
    district: 13
    party: Democrat
  - type: rep
    start: '1995-01-04'
    end: '1997-01-03'
    state: OH
    district: 13
    party: Democrat
  - type: rep
    start: '1997-01-07'
    end: '1999-01-03'
    state: OH
    district: 13
    party: Democrat
  - type: rep
    start: '1999-01-06'
    end: '2001-01-03'
    state: OH
    district: 13
    party: Democrat
  - type: rep
    start: '2001-01-03'
    end: '2003-01-03'
    state: OH
    district: 13
    party: Democrat
  - type: rep
    start: '2003-01-07'
    end: '2005-01-03'
    state: OH
    district: 13
    party: Democrat
    url: http://www.house.gov/sherrodbrown
  - type: rep
    start: '2005-01-04'
    end: '2007-01-03'
    state: OH
    district: 13
    party: Democrat
    url: http://www.house.gov/sherrodbrown
  - type: sen
    start: '2007-01-04'
    end: '2013-01-03'
    state: OH
    class: 1
    party: Democrat
    url: http://brown.senate.gov/
    address: 713 HART SENATE OFFICE BUILDING WASHINGTON DC 20510
    phone: 202-224-2315
    fax: 202-228-6321
    contact_form: http://www.brown.senate.gov/contact/
    office: 713 Hart Senate Office Building
  - type: sen
    start: '2013-01-03'
    end: '2019-01-03'
    state: OH
    party: Democrat
    class: 1
    url: https://www.brown.senate.gov
    address: 713 Hart Senate Office Building Washington DC 20510
    phone: 202-224-2315
    fax: 202-228-6321
    contact_form: http://www.brown.senate.gov/contact/
    office: 713 Hart Senate Office Building
    state_rank: senior
    rss_url: http://www.brown.senate.gov/rss/feeds/?type=all&amp;
- id:
    bioguide: C000127
    thomas: '00172'
    lis: S275
    govtrack: 300018
    opensecrets: N00007836
    votesmart: 27122
    fec:
    - S8WA00194
    - H2WA01054
    cspan: 26137
    wikipedia: Maria Cantwell
    house_history: 10608
    ballotpedia: Maria Cantwell
    maplight: 544
    icpsr: 39310
    wikidata: Q22250
    google_entity_id: kg:/m/01x68t
  name:
    first: Maria
    last: Cantwell
    official_full: Maria Cantwell
  bio:
    birthday: '1958-10-13'
    gender: F
    religion: Roman Catholic
  terms:
  - type: rep
    start: '1993-01-05'
    end: '1995-01-03'
    state: WA
    district: 1
    party: Democrat
  - type: sen
    start: '2001-01-03'
    end: '2007-01-03'
    state: WA
    class: 1
    party: Democrat
    url: http://cantwell.senate.gov
  - type: sen
    start: '2007-01-04'
    end: '2013-01-03'
    state: WA
    class: 1
    party: Democrat
    url: http://cantwell.senate.gov
    address: 311 HART SENATE OFFICE BUILDING WASHINGTON DC 20510
    phone: 202-224-3441
    fax: 202-228-0514
    contact_form: http://www.cantwell.senate.gov/contact/
    office: 311 Hart Senate Office Building
  - type: sen
    start: '2013-01-03'
    end: '2019-01-03'
    state: WA
    party: Democrat
    class: 1
    url: https://www.cantwell.senate.gov
    address: 511 Hart Senate Office Building Washington DC 20510
    phone: 202-224-3441
    fax: 202-228-0514
    contact_form: http://www.cantwell.senate.gov/public/index.cfm/email-maria
    office: 511 Hart Senate Office Building
    state_rank: junior
    rss_url: http://www.cantwell.senate.gov/public/index.cfm/rss/feed
"""


errorToString : Parser.DeadEnd -> String
errorToString deadEnd =
  case deadEnd.problem of
    Parser.Expecting string ->
      "Expecting: " ++ string
    
    Parser.ExpectingInt ->
      "Expecting: Int"
    
    Parser.ExpectingHex ->
      "Expecting: Hex"
    
    Parser.ExpectingOctal ->
      "Expecting: Octal"
    
    Parser.ExpectingBinary ->
      "Expecting: Binary"
    
    Parser.ExpectingFloat ->
      "Expecting: Float"
    
    Parser.ExpectingNumber ->
      "Expecting: Number"
    
    Parser.ExpectingVariable ->
      "Expecting: Variable"
    
    Parser.ExpectingSymbol symbol ->
      "Expecting: Symbol " ++ symbol
    
    Parser.ExpectingKeyword keyword ->
      "Expecting: Keyword " ++ keyword
    
    Parser.ExpectingEnd ->
      "Expecting: End"
    
    Parser.UnexpectedChar ->
      "Expecting: Char"
    
    Parser.Problem problem ->
      "Expecting: " ++ problem
    
    Parser.BadRepeat ->
      "BadRepeat"



-- HELPERS


yamlValueToHtml : Yaml.Value -> Html.Html msg
yamlValueToHtml value =
  Html.text (Yaml.toString value)

