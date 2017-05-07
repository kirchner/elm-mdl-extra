module Main exposing (main)

import Html exposing (Html)
import Material.Extra as Material
    exposing
        ( Views
        , MdlMsg
        , program
        , component
        , onInput
        )
import InputComponent exposing (input)


main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { state0 : Maybe Int
    , state1 : Maybe Int
    }


defaultModel =
    { state0 = Nothing
    , state1 = Nothing
    }


type Msg
    = Update0 (Maybe Int)
    | Update1 (Maybe Int)


init : ( Model, Cmd Msg )
init =
    ( { state0 = Nothing
      , state1 = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update0 newState ->
            { model | state0 = newState } ! []

        Update1 newState ->
            { model | state1 = newState } ! []


view : Model -> Views Msg -> Html (MdlMsg Msg)
view model views =
    Html.div []
        [ views.render [ 0 ] (Material.embed (input Update0) model.state0)
        , Html.text (toString model.state0)
        , views.render [ 1 ] (Material.embed (input Update1) model.state1)
        , Html.text (toString model.state1)
        ]
