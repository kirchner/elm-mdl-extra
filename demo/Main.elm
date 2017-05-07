module Main exposing (main)

import Html exposing (Html)
import Material.Extra exposing (..)


main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



{- actual model -}


type alias Model =
    { state : Maybe Int }


defaultModel =
    { state = Nothing }


type Msg
    = Update (Maybe Int)


init : ( Model, Cmd Msg )
init =
    ( { state = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update newState ->
            { model | state = newState } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Components Msg -> Html (MdlMsg Msg)
view model mdl =
    Html.div []
        [ mdl.textfield [ 0 ]
            [ onInput (Update << Result.toMaybe << String.toInt) ]
            []
        , Html.text (toString model.state)
        ]
