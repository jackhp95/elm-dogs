module Main exposing (main)

{-| Nothing Crazy is being done in this app.

  - It's using the Store Pattern. (<https://elm-radio.com/episode/store-pattern/>)
  - It's using a Dict for the form values. (Similar to: <https://github.com/dillonkearns/elm-form>)
  - It's using AppUrl for routing. (<https://elm-radio.com/episode/elm-app-url/>)

All of this stuff has been covered and discussed by some of the best Elm developers in the community.

-}

import AppUrl
import Browser
import Html exposing (Html)
import Logic exposing (Model, Msg(..), init, update)
import View


asDocument : ( String, Html Msg ) -> Browser.Document Msg
asDocument ( title, page ) =
    { title = title
    , body = [ View.appWrapper [ page ] ]
    }


view : Model -> ( String, Html Msg )
view model =
    case model.url.path of
        [] ->
            ( "home", View.home model )

        [ "dogs" ] ->
            ( "dogs", View.dogs model )

        [ "dogs", breed ] ->
            ( breed, View.dog model breed Nothing )

        [ "dogs", breed, subBreed ] ->
            ( breed, View.dog model breed (Just subBreed) )

        [ "join" ] ->
            ( "join", View.join model )

        _ ->
            ( "404", View.status_404 model )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = asDocument << view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = AppUrl.fromUrl >> UrlChanged
        }
