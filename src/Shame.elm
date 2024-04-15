module Shame exposing (autocomplete, capitalize, decodeErrorToString, decodeNonBlankString, decodingImage, loadingImage, onFormInput)

{-| This module is where I put code that has an uncertain home.

  - Perhaps a external package needs this function to be added to it.
  - Perhaps a function is too specific to be in a general library.
  - Perhaps a function is too general to be in a specific library.

This module is a place to centralize all these questions so we can find more holistic answers later.

-}

import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as JD


capitalize : String -> String
capitalize word =
    String.uncons word
        |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
        |> Maybe.withDefault ""


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


decodeNonBlankString : JD.Decoder String
decodeNonBlankString =
    JD.string
        |> JD.andThen
            (\str ->
                if String.isEmpty (String.trim str) then
                    JD.fail "is required"

                else
                    JD.succeed str
            )


decodeErrorToString : JD.Error -> String
decodeErrorToString error =
    errorToStringHelp error []


errorToStringHelp : JD.Error -> List String -> String
errorToStringHelp error context =
    case error of
        JD.Field f err ->
            errorToStringHelp err (f :: context)

        JD.Index i err ->
            let
                indexName =
                    String.fromInt (i + 1)
            in
            errorToStringHelp err (indexName :: context)

        JD.OneOf errors ->
            case errors of
                [] ->
                    "I'm a teapot"

                [ err ] ->
                    errorToStringHelp err context

                errs ->
                    errs
                        |> List.map decodeErrorToString
                        |> List.sortBy String.length
                        |> List.head
                        |> Maybe.withDefault ""

        JD.Failure msg _ ->
            if String.contains "OBJECT" msg then
                ""

            else
                String.concat (List.reverse context) ++ " " ++ msg
