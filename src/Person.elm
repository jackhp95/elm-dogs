module Person exposing
    ( Validated, Person, person, validate, decoder, toDict, validatedFromDict
    , firstName, lastName, validateName, decodeName
    , MaritalStatus(..), maritalStatus, validateMaritalStatus, decodeMaritalStatus, stringFromMaritalStatus, maritalStatusList
    , PhoneNumber, phoneNumber, validatePhoneNumber, stringFromPhoneNumber, decodePhoneNumber
    , SSN, ssn, validateSSN, stringFromSSN, decodeSSN
    )

{-|


## The Pholosophy behind using a Dict for all Editable inputs.

The editable fields of a `Person` are simply held in a Dict.
Any editable structure beyond that for HTML is a opinionation of structure for UI.
All HTML form elements use strings. Using a Dict of strings is the most direct way to model user input.

If we're trying to make accesible forms, we're going to want to opt for native inputs like radio buttons, checkboxes, etc.
So to interface with the `value` of those inputs, we'll need strings!


### Idiomatic Elm Enum Example

Sure a `MaritalStatus` could be an enum in an editable record, but it's not a string.
So we'll need to convert it to a `String` for the HTML form.
Once a selection is made, we could have a listener on each input and returns the `MaritalStatus` enum.
Sure, that'd handle what we need. And it's not wrong, but what do you pass in? An Enum?
It'll have to be converted to a string to be put in the view. You also need to remember to be semantically correct with the HTML.
Sure, it might be easier to use a `button` or a `div`, but without the proper care, that can rob screenreader users of of semantic input meaning.
There's a lot of little inconveniences that can add up to a lot of work. And for what benefit? There's gotta be a better way...


### What if we use a FormData() like Dict of Strings?

Instead of the more typical a listener per input pattern of elm,
We can use a single listener on the form and listen for `change`, `input`, or `submit` events.
As long as we get the name of the input and the value of the event target we can update the Dict of values.

That subtle pattern shift forces us to write more-a11y HTML code.

  - Your `div` isn't triggering the `onChange`? That's because it's not a form element. The solution is to use semantic HTML.
  - Your `radio` inputs aren't ending up in the right key for `onChange`? You likely forgot the `name` attribute!
  - Your `checkbox` just has a value of "on"? That's because you forgot the `value` attribute!

The browser can do a lot of work for us. We just need to make sure we're giving it the right information.
This pattern allows us to develop a synergistic relationship with the browser and the DOM.


### Merciful Pattern

This pattern shift keeps the changes to `Msg` and `update` really minimal since every Dict input basically shares the exact same Msg.
Most changes will need to occur in the view and decoders/encoders. That's really it!

It also doesn't preclude the use of more typical Elm input patterns. It just makes them the less convenient option.
If there's something that can't be articulated with standard HTML form elements, you can always use a custom element.
And without the clutter of the rest of the form elements, it's easier to see these special exceptions live.

@docs Validated, Person, person, validate, decoder, toDict, validatedFromDict

Any Validation around names is frustrating for folks with uncommon names and an exercise in futility.
firstName and lastName are rather eurocentric, but they're the most common names in the US. I assume that with the SSN, we're in the US.
givenName and familyName could be more inclusive, but firstName and lastName likely maps more directly with the language of the devs and users.
These can also be updated to be more inclusive if the product is used in a more diverse context.

@docs firstName, lastName, validateName, decodeName

Marital Status is a legally recognized status in the US.

@docs MaritalStatus, maritalStatus, validateMaritalStatus, decodeMaritalStatus, stringFromMaritalStatus, maritalStatusList

Phone Numbers can vary is length and format, so we're unopinionated about the format.
We're just making sure it's got the right number of digits. We're also unopinionated about the country code.
I basically just want to make sure it's a valid phone number, something that could be passed into twilio.

@docs PhoneNumber, phoneNumber, validatePhoneNumber, stringFromPhoneNumber, decodePhoneNumber

Social Security Numbers are like phone numbers. They're digit based IDs.
The "Number" qualities of them are entirely unimportant.
Adding up Social Security Numbers does nothing useful.
Sorting by Social Security Numbers is nonsense.
We use strings under the hood to honor the formatting of the user.
However, we can very easily strip that and format it as we wish.
Another upside of this is we don't need to worry about converting to a number and back.
What we store in the DB, is what we put in the view.

@docs SSN, ssn, validateSSN, stringFromSSN, decodeSSN


## Complete Validation

I assumed that the level of validation for these fields would "real-time".
That is, as soon as the user types in a field, the validation is run.

If there's more in depth validation that needs to be done, that can be accounted for.

  - Do we need to check if a phone number actually belongs to the user?
  - Do we need to check if a SSN is associated to the user?
  - Do we need to check if a they're really Single?

All of these things can technically be done, but that's largely out of the scope and spirit of what we're doing here.
I'm not looking for a complete validation of the data, just a good "real-time" one.

-}

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Utils.Decode as UD
import Utils.String


{-| This constructor isn't exposed outside of this Module.
This ensures that the only way to create a Validated Person is through this module.

  - Person.decoder
  - Person.validate
  - Person.validatedFromDict

Since this module is the only one that can create a Validated Person, it's the only one that can deconstruct it.
So getters are exposed for the all of the fields and the Person record itself.

-}
type Validated
    = Validated_ Person


{-| This is the record that holds all of the pertinent information for an Person.
-}
type alias Person =
    { firstName : String
    , lastName : String
    , maritalStatus : MaritalStatus
    , phoneNumber : PhoneNumber -- US Phone Number
    , ssn : SSN -- Social Security Number
    }


{-| Validate a Person type
-}
validate : Person -> Result String Validated
validate =
    toDict >> validatedFromDict


{-| Decode a JSON object into a Validated Person
-}
decoder : JD.Decoder Validated
decoder =
    JD.map5 Person
        (JD.field "first name" decodeName)
        (JD.field "last name" decodeName)
        (JD.field "marital status" decodeMaritalStatus)
        (JD.field "phone number" decodePhoneNumber)
        (JD.field "social security number" decodeSSN)
        |> JD.map Validated_


{-| We could duplicate the Json Decoder API for dicts, or just convert.
Maybe there's a world where there's a perf hit for this work around, but I haven't found it.
-}
validatedFromDict : Dict String String -> Result String Validated
validatedFromDict form =
    form
        |> JE.dict String.toLower JE.string
        |> JD.decodeValue decoder
        |> Result.mapError Utils.String.decodeErrorToString


{-| We could duplicate the Json Decoder API for dicts, or just convert.
Maybe there's a world where there's a perf hit for this work around, but I haven't found it.
-}
toDict : Person -> Dict String String
toDict person_ =
    Dict.fromList
        [ ( "first name", person_.firstName )
        , ( "last name", person_.lastName )
        , ( "marital status", stringFromMaritalStatus person_.maritalStatus )
        , ( "phone number", stringFromPhoneNumber person_.phoneNumber )
        , ( "social security number", stringFromSSN person_.ssn )
        ]


{-| Get the Person record of a Validated Person
-}
person : Validated -> Person
person (Validated_ person_) =
    person_


{-| Get the first name of a Validated Person
-}
firstName : Validated -> String
firstName (Validated_ person_) =
    person_.firstName


{-| Get the last name of a Validated Person
-}
lastName : Validated -> String
lastName (Validated_ person_) =
    person_.lastName


{-| Get the MaritalStatus of a Validated Person
-}
maritalStatus : Validated -> MaritalStatus
maritalStatus (Validated_ person_) =
    person_.maritalStatus


{-| Get the PhoneNumber of a Validated Person
-}
phoneNumber : Validated -> PhoneNumber
phoneNumber (Validated_ person_) =
    person_.phoneNumber


{-| Get the SSN of a Validated Person
-}
ssn : Validated -> SSN
ssn (Validated_ person_) =
    person_.ssn


{-| These are the legally recognised marital statuses in the US.
If I missed any, this can be updated.

This is exposed to allow pattern matching outside of this module.

-}
type MaritalStatus
    = Single
    | Married
    | Divorced
    | Widowed
    | Separated


{-| This is a convenience function to provide a complete list of MaritalStatuses.
-}
maritalStatusList : List MaritalStatus
maritalStatusList =
    [ Single
    , Married
    , Divorced
    , Widowed
    , Separated
    ]


{-| This takes a string and returns an Ok MaritalStatus, or an Err explaining why it isn't.
-}
validateMaritalStatus : String -> Result String MaritalStatus
validateMaritalStatus =
    Ok
        >> Result.andThen Utils.String.isRequired
        >> Result.andThen
            (\str ->
                case Utils.String.enumFriendly str of
                    "single" ->
                        Ok Single

                    "married" ->
                        Ok Married

                    "divorced" ->
                        Ok Divorced

                    "widowed" ->
                        Ok Widowed

                    "separated" ->
                        Ok Separated

                    other ->
                        Err ("of " ++ other ++ " is an invalid marital status")
            )


{-| Decode a json string as a `MaritalStatus`
-}
decodeMaritalStatus : JD.Decoder MaritalStatus
decodeMaritalStatus =
    UD.andThenResult validateMaritalStatus JD.string


{-| Convert a `MaritalStatus` into a `String`
-}
stringFromMaritalStatus : MaritalStatus -> String
stringFromMaritalStatus ms =
    case ms of
        Single ->
            "single"

        Married ->
            "married"

        Divorced ->
            "divorced"

        Widowed ->
            "widowed"

        Separated ->
            "separated"


{-| This is a Social Security Number type.
It's not exposed outside of this module to prevent invalid SSN strings from being constructed and typed as such.
In orfer to create a SSN, you must use `validateSSN` or `decodeSNN`.
-}
type SSN
    = SSN_ String


{-| This ensures that the provided string isn't blank and has the right number of digits.
This module is unopinionated about the format of the SSN. Whatever is most comfortable for the user is presented.
-}
validateSSN : String -> Result String SSN
validateSSN =
    Ok
        >> Result.andThen Utils.String.isRequired
        >> Result.andThen (Utils.String.hasDigitsWithLength 9)
        >> Result.map SSN_


{-| Decode a json string as a `SSN`
-}
decodeSSN : JD.Decoder SSN
decodeSSN =
    UD.andThenResult validateSSN JD.string


{-| Convert a `SSN` into a `String`
-}
stringFromSSN : SSN -> String
stringFromSSN (SSN_ ssn_) =
    ssn_


{-| This is a Phone Number type.
It's not exposed outside of this module to prevent invalid PhoneNumber strings from being constructed and typed as such.
In orfer to create a PhoneNumber, you must use `validatePhoneNumber` or `decodePhoneNumber`.
-}
type PhoneNumber
    = PhoneNumber_ String


{-| This ensures that the provided string isn't blank and has the right number of digits.
-}
validatePhoneNumber : String -> Result String PhoneNumber
validatePhoneNumber =
    Ok
        >> Result.andThen Utils.String.isRequired
        >> Result.andThen (Utils.String.hasDigitCountWithinRange 10 15)
        >> Result.map PhoneNumber_


{-| Decode a json string as a `PhoneNumber`
-}
decodePhoneNumber : JD.Decoder PhoneNumber
decodePhoneNumber =
    UD.andThenResult validatePhoneNumber JD.string


{-| Convert a `PhoneNumber` into a `String`
-}
stringFromPhoneNumber : PhoneNumber -> String
stringFromPhoneNumber (PhoneNumber_ phoneNumber_) =
    phoneNumber_


{-| This validates that a String is a reasonable Length for a name.
If we had a DB constraint around max name langth, that could be accomidated for in this function.
-}
validateName : String -> Result String String
validateName =
    Ok
        >> Result.andThen Utils.String.isRequired
        >> Result.andThen (Utils.String.hasCharCountWithinRange 2 500)


{-| This decodes a name
-}
decodeName : JD.Decoder String
decodeName =
    UD.andThenResult validateName JD.string
