module Person exposing (MaritalStatus, PhoneNumber, SSN, Validated, firstName, maritalStatusList, maritalStatusToString, validatedFromForm)

import Char
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Shame


type MaritalStatus
    = Single
    | Married
    | Divorced
    | Widowed


maritalStatusList : List MaritalStatus
maritalStatusList =
    [ Single
    , Married
    , Divorced
    , Widowed
    ]


maritalStatusFromString : String -> Maybe MaritalStatus
maritalStatusFromString str =
    case String.toLower str of
        "single" ->
            Just Single

        "married" ->
            Just Married

        "divorced" ->
            Just Divorced

        "widowed" ->
            Just Widowed

        _ ->
            Nothing


maritalStatusToString : MaritalStatus -> String
maritalStatusToString ms =
    case ms of
        Single ->
            "single"

        Married ->
            "married"

        Divorced ->
            "divorced"

        Widowed ->
            "widowed"


type Validated
    = Validated_ Person


type alias Person =
    { firstName : String
    , lastName : String
    , maritalStatus : MaritalStatus
    , phoneNumber : PhoneNumber -- US Phone Number
    , ssn : SSN -- Social Security Number
    }


type SSN
    = SSN_ String


type PhoneNumber
    = PhoneNumber_ String


decodeSSN : JD.Decoder SSN
decodeSSN =
    Shame.decodeNonBlankString
        |> JD.map (String.filter Char.isDigit)
        |> JD.andThen
            (\str ->
                if String.length str == 9 then
                    JD.succeed str

                else
                    JD.fail "doesn't have 9 digits"
            )
        |> JD.map SSN_


decodeMaritalStatus : JD.Decoder MaritalStatus
decodeMaritalStatus =
    JD.andThen
        (\str ->
            case maritalStatusFromString str of
                Just ms ->
                    JD.succeed ms

                Nothing ->
                    JD.fail "is unknown"
        )
        Shame.decodeNonBlankString


decodePhoneNumber : JD.Decoder PhoneNumber
decodePhoneNumber =
    Shame.decodeNonBlankString
        |> JD.map (String.filter Char.isDigit)
        |> JD.andThen
            (\str ->
                if String.length str == 10 then
                    JD.succeed str

                else
                    JD.fail "doesn't have 10 digits"
            )
        |> JD.map PhoneNumber_


decoder : JD.Decoder Validated
decoder =
    JD.map5 Person
        (JD.field "first name" Shame.decodeNonBlankString)
        (JD.field "last name" Shame.decodeNonBlankString)
        (JD.field "marital status" decodeMaritalStatus)
        (JD.field "us phone number" decodePhoneNumber)
        (JD.field "social security number" decodeSSN)
        |> JD.map Validated_


firstName : Validated -> String
firstName (Validated_ person_) =
    person_.firstName


{-| We could duplicate the Json Decoder API for dicts, or just convert.
Maybe there's a world where there's a perf hit for this work around, but I haven't found it.
-}
validatedFromForm : Dict String String -> Result String Validated
validatedFromForm form =
    form
        |> JE.dict String.toLower JE.string
        |> JD.decodeValue decoder
        |> Result.mapError Shame.decodeErrorToString
