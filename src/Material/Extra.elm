module Material.Extra
    exposing
        ( program
        , Components
        , MdlMsg
        , onInput
        )

import Material
import Material.Options as Options
import Material.Textfield as Textfield
import Html exposing (Html)


type alias MdlModel model =
    { mdl : Material.Model
    , userModel : model
    }


type MdlMsg msg
    = MdlMsg (Material.Msg (MdlMsg msg))
    | UserMsg msg


type alias Components msg =
    { textfield :
        List Int
        -> List (Textfield.Property (MdlMsg msg))
        -> List (Html (MdlMsg msg))
        -> Html (MdlMsg msg)
    }


onInput : (String -> msg) -> Textfield.Property (MdlMsg msg)
onInput callback =
    Options.onInput (UserMsg << callback)


program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Components msg -> Html (MdlMsg msg)
    }
    -> Program Never (MdlModel model) (MdlMsg msg)
program { init, update, subscriptions, view } =
    Html.program
        { init = liftInit init
        , update = liftUpdate update
        , subscriptions = liftSubscriptions subscriptions
        , view = liftView view
        }


liftInit : ( model, Cmd msg ) -> ( MdlModel model, Cmd (MdlMsg msg) )
liftInit ( userModel, userCmd ) =
    let
        cmds =
            Cmd.batch
                [ Cmd.map UserMsg userCmd
                , Material.init MdlMsg
                ]
    in
        ( { userModel = userModel, mdl = Material.model }, cmds )


liftUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> MdlMsg msg
    -> MdlModel model
    -> ( MdlModel model, Cmd (MdlMsg msg) )
liftUpdate userUpdate msg mdlModel =
    case msg of
        MdlMsg mdlMsg ->
            Material.update MdlMsg mdlMsg mdlModel

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
        , Material.subscriptions MdlMsg mdlModel
        ]


liftView : (model -> Components msg -> Html (MdlMsg msg)) -> MdlModel model -> Html (MdlMsg msg)
liftView userView model =
    let
        interface =
            { textfield = textfield }

        textfield index attrs nodes =
            Textfield.render MdlMsg index model.mdl attrs nodes
    in
        userView model.userModel interface
