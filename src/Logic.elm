port module Logic exposing (Model, Msg(..), init, update)

import AppUrl exposing (AppUrl)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Dict exposing (Dict)
import DogCeo
import Json.Encode as JE
import Store exposing (..)
import Url exposing (Url)


port scrollIntoView : JE.Value -> Cmd msg


smoothScroll : Maybe String -> Cmd msg
smoothScroll maybeFragment =
    maybeFragment
        |> Maybe.map
            (\fragment ->
                [ ( "id", JE.string fragment ) ]
                    |> JE.object
                    |> scrollIntoView
            )
        |> Maybe.withDefault Cmd.none


type alias Model =
    { url : AppUrl
    , navKey : Key
    , form : Dict String String
    , store : Store
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    onUrlChange
        { url = AppUrl.fromUrl url
        , navKey = key
        , form = Dict.empty
        , store = Store.init
        }


onUrlChange : Model -> ( Model, Cmd Msg )
onUrlChange model =
    case model.url.path of
        [ "dogs" ] ->
            DogCeo.requireAllBreeds model.store
                |> mapStoreTuple model

        "dogs" :: breed :: tail ->
            let
                maybeSubBreed =
                    List.head tail
            in
            DogCeo.requireBreed breed maybeSubBreed model.store
                |> mapStoreTuple model

        _ ->
            ( model, Cmd.none )


type Msg
    = FormInput (Dict String String)
    | StoreMsg Store.Msg
    | UrlRequested UrlRequest
    | UrlChanged AppUrl


mapStoreTuple : Model -> ( Store, Cmd Store.Msg ) -> ( Model, Cmd Msg )
mapStoreTuple model =
    Tuple.mapBoth
        (\store -> { model | store = store })
        (Cmd.map StoreMsg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormInput form ->
            ( { model | form = Dict.union form model.form }, Cmd.none )

        StoreMsg storeMsg ->
            Store.update storeMsg model.store
                |> mapStoreTuple model

        UrlRequested (External string) ->
            ( model, Nav.load string )

        UrlRequested (Internal url) ->
            let
                maybeHashScroll =
                    if (AppUrl.fromUrl >> .fragment >> (/=) model.url.fragment) url then
                        smoothScroll url.fragment

                    else
                        Cmd.none
            in
            ( model
            , Cmd.batch
                [ Nav.pushUrl model.navKey (Url.toString url)
                , maybeHashScroll
                ]
            )

        UrlChanged url ->
            onUrlChange { model | url = url }
