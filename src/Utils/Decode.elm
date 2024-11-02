module Utils.Decode exposing (..)

import Json.Decode as JD


andThenResult : (a -> Result String b) -> JD.Decoder a -> JD.Decoder b
andThenResult fn =
    JD.andThen
        (\val ->
            case fn val of
                Ok a ->
                    JD.succeed a

                Err msg ->
                    JD.fail msg
        )
