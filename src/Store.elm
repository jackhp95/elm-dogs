module Store exposing (Error(..), Msg(..), Store(..), StoreValue, get, init, require, update)

import Dict exposing (Dict)
import Http
import Json.Decode as JD
import RemoteData exposing (RemoteData)


type Msg
    = Fetched String (RemoteData Error JD.Value)


{-| By Making this opaque, we can ensure that devs won't update values.
-}
type Store
    = Store_ (Dict String StoreValue)


type alias StoreValue =
    RemoteData Error JD.Value


type Error
    = HttpError Http.Error
    | JsonError JD.Error


{-| This is a declarative way to describe what data a route requires.
-}
require : String -> Store -> ( Store, Cmd Msg )
require url (Store_ store) =
    Dict.get url store
        |> Maybe.andThen
            (\storeValue ->
                if RemoteData.isSuccess storeValue || RemoteData.isLoading storeValue then
                    -- If the store value exists or is loading,
                    -- we don't need to fetch it again.
                    Just ( Store_ store, Cmd.none )

                else
                    Nothing
            )
        -- Otherwise, fetch the URL.
        |> Maybe.withDefault
            ( Store_ (Dict.insert url RemoteData.Loading store)
            , Http.get
                { url = url
                , expect =
                    Http.expectJson
                        (RemoteData.fromResult
                            >> RemoteData.mapError HttpError
                            >> Fetched url
                        )
                        JD.value
                }
            )


get : String -> JD.Decoder a -> Store -> RemoteData Error a
get url decoder (Store_ store) =
    Dict.get url store
        |> Maybe.map
            -- If there's an error,
            (RemoteData.andThen
                (JD.decodeValue decoder
                    >> RemoteData.fromResult
                    >> RemoteData.mapError JsonError
                )
            )
        |> Maybe.withDefault RemoteData.NotAsked


init : Store
init =
    Store_ Dict.empty


update : Msg -> Store -> ( Store, Cmd Msg )
update msg (Store_ store) =
    case msg of
        Fetched url value ->
            ( Store_ (Dict.insert url value store), Cmd.none )
