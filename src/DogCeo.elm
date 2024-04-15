module DogCeo exposing (AllBreeds, Breed, getAllBreeds, getBreed, requireAllBreeds, requireBreed)

import Dict exposing (Dict)
import Json.Decode as JD
import RemoteData exposing (..)
import Store exposing (..)



-- All Breeds


type alias AllBreeds =
    Dict String (List String)


requireAllBreeds : Store -> ( Store, Cmd Msg )
requireAllBreeds =
    require allBreedsUrl


getAllBreeds : Store -> RemoteData Store.Error AllBreeds
getAllBreeds =
    Store.get allBreedsUrl decodeAllBreeds


allBreedsUrl : String
allBreedsUrl =
    "https://dog.ceo/api/breeds/list/all"


decodeAllBreeds : JD.Decoder AllBreeds
decodeAllBreeds =
    JD.field "message" (JD.dict (JD.list JD.string))



-- Breed


type alias Breed =
    List String


requireBreed : String -> Maybe String -> Store -> ( Store, Cmd Msg )
requireBreed breed maybeSubBreed =
    require (breedUrl breed maybeSubBreed)


getBreed : String -> Maybe String -> Store -> RemoteData Store.Error Breed
getBreed breed maybeSubBreed =
    Store.get (breedUrl breed maybeSubBreed) decodeBreed


breedUrl : String -> Maybe String -> String
breedUrl breed maybeSubBreed =
    case maybeSubBreed of
        Nothing ->
            "https://dog.ceo/api/breed/" ++ breed ++ "/images"

        Just subBreed ->
            "https://dog.ceo/api/breed/" ++ breed ++ "/" ++ subBreed ++ "/images"


decodeBreed : JD.Decoder Breed
decodeBreed =
    JD.field "message" (JD.list JD.string)
