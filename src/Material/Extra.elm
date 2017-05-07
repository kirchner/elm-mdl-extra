module Material.Extra
    exposing
        ( program
        , component
        , Views
        , MdlMsg
        , onInput
        , embed
        )

import Dict exposing (Dict)
import Material
import Material.Options as Options
import Material.Textfield as Textfield
import Html exposing (Html)


type alias MdlModel model =
    { mdl : Material.Model
    , userModel : model
    , mdls : Dict (List Int) Material.Model
    }


type MdlMsg msg
    = MdlMsg (Maybe (List Int)) (Material.Msg (MdlMsg msg))
    | UserMsg msg


type alias Views msg =
    { textfield :
        List Int
        -> List (Textfield.Property (MdlMsg msg))
        -> List (Html (MdlMsg msg))
        -> Html (MdlMsg msg)
    , render :
        List Int -> (Material.Model -> List Int -> Html (MdlMsg msg)) -> Html (MdlMsg msg)
    }


embed :
    Component model msg
    -> model
    -> (Material.Model -> List Int -> Html (MdlMsg msg))
embed component model mdl index =
    component.view index { userModel = model, mdl = mdl, mdls = Dict.empty }


onInput : (String -> msg) -> Textfield.Property (MdlMsg msg)
onInput callback =
    Options.onInput (UserMsg << callback)


program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Views msg -> Html (MdlMsg msg)
    }
    -> Program Never (MdlModel model) (MdlMsg msg)
program { init, update, subscriptions, view } =
    Html.program
        { init = liftInit init
        , update = liftUpdate update
        , subscriptions = liftSubscriptions subscriptions
        , view = liftView view Nothing
        }


component :
    { -- init : ( model, Cmd msg )
      -- , update : msg -> model -> ( model, Cmd msg )
      -- , subscriptions : model -> Sub msg
      view : model -> Views msg -> Html (MdlMsg msg)
    }
    -> Component model msg
component { view } =
    -- { init, update, subscriptions, view } =
    { -- init = liftInit init
      -- , update = liftUpdate update
      -- , subscriptions = liftSubscriptions subscriptions
      view = \mdlIndex -> liftView view (Just mdlIndex)
    }


type alias Component model msg =
    { -- init : ( MdlModel model, Cmd (MdlMsg msg) )
      -- , update : MdlMsg msg -> MdlModel model -> ( MdlModel model, Cmd (MdlMsg msg) )
      -- , subscriptions : MdlModel model -> Sub (MdlMsg msg)
      view : List Int -> MdlModel model -> Html (MdlMsg msg)
    }


liftInit : ( model, Cmd msg ) -> ( MdlModel model, Cmd (MdlMsg msg) )
liftInit ( userModel, userCmd ) =
    let
        cmds =
            Cmd.batch
                [ Cmd.map UserMsg userCmd
                , Material.init (MdlMsg Nothing)
                ]
    in
        ( { userModel = userModel
          , mdl = Material.model
          , mdls = Dict.empty
          }
        , cmds
        )


liftUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> MdlMsg msg
    -> MdlModel model
    -> ( MdlModel model, Cmd (MdlMsg msg) )
liftUpdate userUpdate msg mdlModel =
    case Debug.log "msg" msg of
        MdlMsg maybeMdlIndex mdlMsg ->
            let
                mdl =
                    case maybeMdlIndex of
                        Just mdlIndex ->
                            Dict.get mdlIndex mdlModel.mdls
                                |> Maybe.withDefault Material.model

                        Nothing ->
                            mdlModel.mdl

                ( newMdl, cmds ) =
                    Material.update (MdlMsg maybeMdlIndex) mdlMsg { mdl = mdl }
            in
                ( { mdlModel
                    | mdl =
                        if maybeMdlIndex == Nothing then
                            newMdl.mdl
                        else
                            mdlModel.mdl
                    , mdls =
                        case maybeMdlIndex of
                            Just mdlIndex ->
                                Dict.insert mdlIndex newMdl.mdl mdlModel.mdls

                            Nothing ->
                                mdlModel.mdls
                  }
                , cmds
                )

        UserMsg userMsg ->
            let
                ( newUserModel, newUserCmd ) =
                    userUpdate userMsg mdlModel.userModel
            in
                ( { mdlModel | userModel = newUserModel }
                , Cmd.map UserMsg newUserCmd
                )


liftSubscriptions :
    (model -> Sub msg)
    -> MdlModel model
    -> Sub (MdlMsg msg)
liftSubscriptions userSubscription mdlModel =
    Sub.batch
        [ Sub.map UserMsg (userSubscription mdlModel.userModel)
        , Material.subscriptions (MdlMsg Nothing) mdlModel
        ]


liftView : (model -> Views msg -> Html (MdlMsg msg)) -> Maybe (List Int) -> MdlModel model -> Html (MdlMsg msg)
liftView userView maybeMdlIndex model =
    let
        interface =
            { textfield = textfield
            , render = render
            }

        mdl =
            case maybeMdlIndex of
                Just mdlIndex ->
                    Dict.get mdlIndex model.mdls
                        |> Maybe.withDefault Material.model

                Nothing ->
                    model.mdl

        textfield index attrs nodes =
            Textfield.render (MdlMsg maybeMdlIndex) index mdl attrs nodes

        render mdlIndex makeHtml =
            makeHtml mdl mdlIndex
    in
        userView model.userModel interface
