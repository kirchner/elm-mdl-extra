module InputComponent exposing (input)

import Html exposing (Html)
import Material.Extra as Material
    exposing
        ( Views
        , MdlMsg
        , component
        , onInput
        )


type alias Model =
    Maybe Int


input callback =
    component
        { -- init = ( {}, Cmd.none )
        -- , update = \_ _ -> ( {}, Cmd.none )
        -- , subscriptions = \_ -> Sub.none
          view = view callback
        }


view : (Maybe Int -> msg) -> Model -> Views msg -> Html (MdlMsg msg)
view callback model mdl =
    Html.div []
        [ mdl.textfield [ 0 ]
            [ onInput (callback << Result.toMaybe << String.toInt) ]
            []
        ]
