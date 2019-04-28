module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, form, input, text)
import Html.Attributes exposing (..)
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { description : String }


type Msg
    = SetDescription String
    | AddEntry


init : () -> ( Model, Cmd Msg )
init _ =
    ( { description = "" }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDescription textfieldValue ->
            ( { model | description = textfieldValue }, Cmd.none )

        AddEntry ->
            let
                cleanDescription =
                    String.trim model.description
            in
            if String.isEmpty cleanDescription then
                ( model, Cmd.none )

            else
                Debug.log cleanDescription ( { model | description = "" }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view mod =
    Html.form [ Events.onSubmit AddEntry ]
        [ input
            [ type_ "text"
            , autofocus True
            , placeholder "What needs to be done?"
            , value mod.description
            , Events.onInput SetDescription
            ]
            []
        ]
