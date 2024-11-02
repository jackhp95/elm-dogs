module Utils.String exposing (..)

import Json.Decode as JD
import Utils.Decode as UD


enumFriendly : String -> String
enumFriendly =
    String.trim >> String.toLower >> String.replace " " "-"


capitalize : String -> String
capitalize word =
    String.uncons word
        |> Maybe.map (\( head, tail ) -> String.cons (Char.toUpper head) tail)
        |> Maybe.withDefault ""


keepDigits : String -> String
keepDigits =
    String.filter Char.isDigit


isRequired : String -> Result String String
isRequired str =
    case String.trim str of
        "" ->
            Err "is required"

        _ ->
            Ok str


hasCharOfTypeWithLength : ( String -> String, String ) -> Int -> String -> Result String String
hasCharOfTypeWithLength ( fn, desc ) count str =
    let
        charCount =
            String.length (fn str)
    in
    if charCount == count then
        Ok str

    else
        Err ("must have exactly " ++ String.fromInt count ++ " " ++ desc)


hasCharsWithLength : Int -> String -> Result String String
hasCharsWithLength =
    hasCharOfTypeWithLength ( identity, "characters" )


hasDigitsWithLength : Int -> String -> Result String String
hasDigitsWithLength =
    hasCharOfTypeWithLength ( keepDigits, "digits" )


hasCharsOfTypeWithinRange : ( String -> String, String ) -> Int -> Int -> String -> Result String String
hasCharsOfTypeWithinRange ( fn, desc ) a b str =
    let
        ( min, max ) =
            if a < b then
                ( a, b )

            else
                ( b, a )

        charCount =
            String.length (fn str)
    in
    case ( min <= charCount, charCount <= max ) of
        ( True, True ) ->
            Ok str

        ( False, True ) ->
            Err ("is too short, must be more than " ++ String.fromInt (min - 1) ++ " " ++ desc)

        ( True, False ) ->
            Err ("is too long, must be less than " ++ String.fromInt (max + 1) ++ " " ++ desc)

        ( False, False ) ->
            Err "is impossible"


hasCharCountWithinRange : Int -> Int -> String -> Result String String
hasCharCountWithinRange =
    hasCharsOfTypeWithinRange ( identity, "characters" )


hasDigitCountWithinRange : Int -> Int -> String -> Result String String
hasDigitCountWithinRange =
    hasCharsOfTypeWithinRange ( keepDigits, "digits" )


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
