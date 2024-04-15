module View exposing (appWrapper, dog, dogs, home, join, status_404)

import Dict
import DogCeo
import Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Http
import List.Extra
import Logic exposing (Model, Msg)
import Person
import RemoteData exposing (RemoteData)
import Shame
import Store


unwrapStore : (a -> Html msg) -> RemoteData Store.Error a -> Html msg
unwrapStore f remoteData =
    case remoteData of
        RemoteData.Loading ->
            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                [ Html.span [ Attr.class "animate-spin text-8xl" ] [ Html.text "🎾" ]
                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "Fetching!" ]
                ]

        RemoteData.NotAsked ->
            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                [ Html.span [ Attr.class "animate-bounce text-8xl" ] [ Html.text "🎾" ]
                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "We've dropped the ball!" ]
                ]

        RemoteData.Failure error ->
            case error of
                Store.HttpError httpError ->
                    case httpError of
                        Http.BadUrl _ ->
                            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                [ Html.span [ Attr.class "text-8xl" ] [ Html.text "🦝" ]
                                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "I don't know that trick" ]
                                ]

                        Http.Timeout ->
                            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                [ Html.span [ Attr.class "text-8xl" ] [ Html.text "⏳" ]
                                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "The request took too long." ]
                                ]

                        Http.NetworkError ->
                            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                [ Html.span [ Attr.class "text-8xl" ] [ Html.text "🦮" ]
                                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "Make sure you're connected." ]
                                ]

                        Http.BadStatus status ->
                            case status // 100 of
                                4 ->
                                    Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                        [ Html.span [ Attr.class "text-8xl" ] [ Html.text "🦴" ]
                                        , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "What're diggin' into?" ]
                                        ]

                                5 ->
                                    Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                        [ Html.span [ Attr.class "animate-bounce text-8xl" ] [ Html.text "🎾" ]
                                        , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "We've dropped the ball!" ]
                                        ]

                                _ ->
                                    Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                        [ Html.span [ Attr.class "animate-spin text-8xl" ] [ Html.text "⁉️" ]
                                        , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "We don't know that trick!" ]
                                        ]

                        Http.BadBody _ ->
                            Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                                [ Html.span [ Attr.class "animate-bounce text-8xl" ] [ Html.text "⚾️" ]
                                , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "We've dropped the ball!" ]
                                ]

                Store.JsonError _ ->
                    Html.div [ Attr.class "mx-auto my-24 flex flex-col items-center gap-4" ]
                        [ Html.span [ Attr.class "animate-spin text-8xl" ] [ Html.text "🌭" ]
                        , Html.h1 [ Attr.class "text-xl opacity-70" ] [ Html.text "Wrong dog!" ]
                        ]

        RemoteData.Success value ->
            f value


appWrapper : List (Html Msg) -> Html Msg
appWrapper content =
    Html.div [ Attr.class "flex flex-col gap-4 min-h-dvh" ]
        (navbar :: content)


navbar : Html Msg
navbar =
    Html.div [ Attr.class "sticky top-0 border-b border-[Field] backdrop-blur z-10" ]
        [ Html.nav [ Attr.class "w-full max-w-3xl mx-auto flex items-center" ]
            [ Html.a [ Attr.href "/", Attr.class "px-4 self-stretch flex items-center hover:bg-[Field] text-4xl mr-auto" ]
                [ Html.text "🐶"
                , Html.span
                    [ Attr.class "sr-only" ]
                    [ Html.text "Home" ]
                ]
            , Html.a [ Attr.href "/dogs", Attr.class "p-4 hover:bg-[Field]" ] [ Html.text "Dogs" ]
            , Html.a [ Attr.href "/join", Attr.class "p-4 hover:bg-[Field]" ] [ Html.text "Join" ]
            ]
        ]


home : Model -> Html Msg
home model =
    Html.main_ [ Attr.class "flex flex-col items-center gap-4 max-w-3xl px-4 mx-auto w-full" ]
        [ Html.h1 [ Attr.class "mt-12 text-6xl" ] [ Html.text "Welcome" ]
        , Html.nav [ Attr.class "flex gap-4" ]
            [ Html.a [ Attr.href "/dogs", Attr.class "border opacity-70 hover:opacity-100 px-4 py-2 rounded hover:bg-[Field]" ] [ Html.text "Dogs" ]
            , Html.a [ Attr.href "/join", Attr.class "border opacity-70 hover:opacity-100 px-4 py-2 rounded hover:bg-[Field]" ] [ Html.text "Join" ]
            ]
        ]


dogs : Model -> Html Msg
dogs model =
    Html.main_ [ Attr.class "flex flex-col gap-4 max-w-3xl px-4 mx-auto w-full" ]
        [ Html.h1 [ Attr.class "mt-12 text-6xl" ] [ Html.text "Dogs" ]
        , DogCeo.getAllBreeds model.store
            |> unwrapStore allBreedsView
        ]


allBreedsView : DogCeo.AllBreeds -> Html msg
allBreedsView allBreeds =
    allBreeds
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( breed, subBreeds ) ->
                Html.li [ Attr.class "contents" ]
                    [ [ Html.a
                            [ Attr.href ("/dogs/" ++ breed)
                            , Attr.class "block px-2 border-[Field] opacity-70 hover:opacity-100 border rounded-full hover:bg-[Field]"
                            ]
                            [ Html.text breed ]
                      ]
                        |> Html.h2 [ Attr.class "contents" ]
                    , subBreeds
                        |> List.sort
                        |> List.map
                            (\subBreed ->
                                [ Html.a
                                    [ [ "", "dogs", breed, subBreed ]
                                        |> String.join "/"
                                        |> Attr.href
                                    , Attr.class "block px-2 border-[Field] opacity-70 hover:opacity-100 border rounded-full hover:bg-[Field]"
                                    ]
                                    [ Html.text breed, Html.text " ", Html.text subBreed ]
                                ]
                                    |> Html.li [ Attr.class "contents" ]
                            )
                        |> Html.ul [ Attr.class "contents" ]
                    ]
            )
        |> Html.ul [ Attr.class "flex flex-wrap gap-2 text-center capitalize" ]


dog : Model -> String -> Maybe String -> Html Msg
dog model breedName maybeSubBreedName =
    let
        breedData =
            DogCeo.getBreed breedName maybeSubBreedName model.store
    in
    Html.main_ [ Attr.class "flex flex-col" ]
        [ Html.h1
            [ Attr.class "overflow-x-auto max-w-3xl capitalize mt-12 py-2 px-4 mx-auto w-full text-6xl" ]
            [ Html.text breedName
            , Html.text " "
            , Maybe.map Html.text maybeSubBreedName
                |> Maybe.withDefault (Html.text "")
            ]
        , unwrapStore (breedView model) breedData
        ]


breedView : Model -> DogCeo.Breed -> Html msg
breedView model breed =
    let
        totalBreeds =
            List.length breed

        totalPages =
            ceiling (toFloat totalBreeds / 20)
    in
    [ Html.p [ Attr.class "max-w-3xl px-4 mx-auto w-full" ]
        [ Html.text "Image Count: "
        , totalBreeds |> String.fromInt |> Html.text
        ]
    , breed
        |> List.Extra.greedyGroupsOf 20
        |> List.indexedMap
            (\index pageBreedImgs ->
                let
                    ( back, curr, next ) =
                        ( index
                        , 1 + index
                        , 2 + index
                        )

                    hashLink pageNum pageName =
                        let
                            numStr =
                                String.fromInt pageNum
                        in
                        Html.a
                            [ Attr.href ("#" ++ numStr)
                            , Attr.classList [ ( "invisible", 1 > pageNum || pageNum > totalPages ) ]
                            , Attr.class "p-4 hover:bg-[Field]"
                            ]
                            [ Html.text pageName ]
                in
                [ pageBreedImgs
                    |> List.map
                        (\breedImg ->
                            [ Html.img
                                [ Attr.alt ""
                                , Shame.loadingImage "lazy"
                                , Shame.decodingImage "async"
                                , Attr.src breedImg
                                , Attr.tabindex 0
                                , Attr.height 100
                                , Attr.width 100
                                , Attr.class "max-h-24 max-w-48 md:max-h-32 md:max-w-64"
                                , Attr.class "focus:max-h-none focus:max-w-none"
                                , Attr.class "min-w-0 w-full h-full focus:max-w-svw focus:max-h-svh"
                                , Attr.class "object-cover focus:object-scale-down"
                                , Attr.class "focus:fixed bottom-0 inset-x-0 focus:z-10"
                                , Attr.class "active:scale-100 hover:scale-105"
                                , Attr.class "focus:pointer-events-none transition-all"
                                , Attr.class "bg-gradient-to-t from-[Canvas]"
                                ]
                                []
                            ]
                                |> Html.li
                                    [ Attr.class "flex-auto bg-gray-500/20"
                                    , Attr.class "max-h-24 max-w-48 md:max-h-32 md:max-w-64"
                                    , Attr.class "focus-within:overflow-visible overflow-hidden"
                                    ]
                        )
                    |> Html.ul
                        [ Attr.id (String.fromInt curr)
                        , Attr.class "w-full scroll-my-20 max-w-3xl md:px-4 mx-auto flex flex-wrap gap-px m-px mb-20"
                        ]
                , [ hashLink back "Back"
                  , hashLink curr ("Page " ++ String.fromInt curr)
                  , hashLink next "Next"
                  ]
                    |> Html.nav [ Attr.class "flex justify-between gap-4 w-full max-w-3xl mx-auto" ]
                    |> List.singleton
                    |> Html.div
                        [ Attr.hidden (String.fromInt curr /= Maybe.withDefault "1" model.url.fragment)
                        , Attr.class "w-full fixed inset-x-0 bottom-0 border-t border-[Field] backdrop-blur z-10"
                        ]
                ]
                    |> Html.li [ Attr.class "flex-none flex flex-col justify-between gap-4 max-w-full min-w-full snap-center snap-always" ]
            )
        |> Html.ul [ Attr.class "flex-auto max-w-full overflow-x-hidden flex items-stretch snap-x snap-mandatory" ]
    ]
        |> Html.section [ Attr.class "flex-auto flex flex-col gap-4" ]


join : Model -> Html Msg
join model =
    let
        field name attrs =
            Html.label [ Attr.class "flex flex-col flex-auto gap-1" ]
                [ Html.span [ Attr.class "text-sm opacity-70" ] [ Html.text name ]
                , Html.input
                    (attrs
                        |> (::) (Attr.class "border-[Field] border rounded px-4 py-2")
                        |> (::) (Attr.name name)
                        |> (::) (Attr.value (Dict.get name model.form |> Maybe.withDefault ""))
                    )
                    []
                ]
    in
    Html.form
        [ Attr.class "max-w-3xl p-4 mx-auto flex flex-col gap-4"
        , Shame.onFormInput Logic.FormInput
        ]
        [ Html.header [ class "flex flex-col gap-2" ]
            [ Html.h1 [ Attr.class "mt-12 text-6xl" ] [ Html.text "Join" ]
            , Html.p [ Attr.class "text-lg opacity-70" ]
                [ Html.text "Please fill out the form below." ]
            ]
        , Html.div [ Attr.class "flex flex-wrap gap-4" ]
            [ field "First Name"
                [ Attr.required True
                , Attr.size 10
                , Shame.autocomplete "given-name"
                ]
            , field "Last Name"
                [ Attr.required True
                , Attr.size 10
                , Shame.autocomplete "family-name"
                ]
            ]
        , Html.div [ Attr.class "flex flex-wrap gap-4" ]
            [ Html.label [ Attr.class "flex flex-col flex-auto gap-1" ]
                [ Html.span [ Attr.class "text-sm opacity-70" ] [ Html.text "Marital Status" ]
                , Person.maritalStatusList
                    |> List.map Person.maritalStatusToString
                    |> List.map
                        (\value ->
                            Html.option
                                [ Attr.value value ]
                                [ Html.text (Shame.capitalize value) ]
                        )
                    |> (::)
                        (Html.option
                            [ Attr.value ""
                            , Attr.disabled True
                            , Attr.hidden True
                            , Attr.selected True
                            ]
                            [ Html.text "—" ]
                        )
                    |> Html.select
                        [ Attr.required True
                        , Attr.value (Dict.get "Marital Status" model.form |> Maybe.withDefault "")
                        , Attr.class "default:opacity-50"
                        , Attr.class "border-[Field] border rounded px-4 py-2 w-32 self-stretch flex-auto"
                        , Attr.name "Marital Status"
                        ]
                ]
            , field "US Phone Number"
                [ Attr.required True
                , Shame.autocomplete "tel-national"
                , Attr.size 14
                , Attr.placeholder "###-###-####"
                ]
            ]
        , field "Social Security Number"
            [ Attr.required True
            , Attr.size 13
            , Attr.placeholder "###-##-####"
            , Attr.class "tabular-nums tracking-wide"
            ]
        , let
            validation =
                Person.validatedFromForm model.form
          in
          Html.footer [ Attr.class "flex flex-col gap-4" ]
            [ (case validation of
                Ok validatedPerson ->
                    [ Html.text (Person.firstName validatedPerson ++ ", it looks like you're ready to submit.") ]

                Err "" ->
                    []

                Err errors ->
                    [ Html.text errors ]
              )
                |> Html.p [ Attr.class "capitalize text-sm px-4 py-2 rounded border text-[GrayText] border-[field]" ]
            , Html.button
                [ Attr.type_ "submit"
                , validation
                    |> Result.map (\_ -> False)
                    |> Result.withDefault True
                    |> Attr.disabled
                , Attr.class "border-[Field] w-full text-center border rounded px-4 py-2"
                , Attr.class "disabled:cursor-not-allowed"
                , Attr.class "enabled:bg-[CanvasText] enabled:text-[Canvas]"
                ]
                [ Html.text "Submit" ]
            ]
        ]


status_404 : Model -> Html Msg
status_404 model =
    Html.text "status_404"
