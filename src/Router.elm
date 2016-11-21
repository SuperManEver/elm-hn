module Router exposing (toHash, hashParser, Route(..), defaultRoute, pageParser)

import Navigation
import UrlParser as Url exposing (Parser, (</>), format, int, oneOf, s, string)
import String

toHash : Route -> String
toHash route =
  case route of
    Home ->
      "#home"

    Saved -> 
      "#saved"    

hashParser : Navigation.Location -> Result String Route 
hashParser location = 
  Url.parse identity pageParser (String.dropLeft 1 location.hash)

type Route 
  = Home 
  | Saved

defaultRoute : Route 
defaultRoute = Home

pageParser : Parser (Route -> a) a
pageParser =
  oneOf
    [ format Home (Url.s "home")
    , format Saved (Url.s "saved")
    ]

