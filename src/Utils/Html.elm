module Utils.Html exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as JD


{-| All HTML form inputs are fundamentally strings.
No need to complect the modeling for user input.
Just keep a dict of what [values] the user has entered for each [name].

We're opting for a Dict.Dict instead JD.Value to make it easier to arbitrarily access the values.

-}
onFormInput : (Dict String String -> msg) -> Html.Attribute msg
onFormInput toMsg =
    JD.map2 Tuple.pair
        (JD.field "name" JD.string)
        (JD.field "value" JD.string)
        |> JD.dict
        |> JD.map Dict.values
        |> JD.at [ "target", "form", "elements" ]
        |> JD.map Dict.fromList
        |> Html.Events.on "input"
        |> Attr.map toMsg


{-| It's not only a boolean, it also takes a "token"
<https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes/autocomplete>
-}
autocomplete : String -> Html.Attribute msg
autocomplete =
    Attr.attribute "autocomplete"


{-|

    <img> tag loading attribute
    "lazy" -> Natively Lazy Load Images
    "eager" -> Prioritize the loading of this image
    "auto" -> Default behavior

-}
loadingImage : String -> Html.Attribute msg
loadingImage =
    Attr.attribute "loading"


{-|

    <img> tag decoding attribute
    "async" -> Don't bother the main thread with loading
    "sync" -> Load after the main thread is done
    "auto" -> Default behavior

-}
decodingImage : String -> Html.Attribute msg
decodingImage =
    Attr.attribute "decoding"
