module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { description : String, entries: List String }


type Msg
    = SetDescription String
    | AddEntry


init : () -> ( Model, Cmd Msg )
init _ =
    ( { description = "", entries = [] }
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
                ( { model | description = ""
                  , entries = model.entries ++ [cleanDescription] }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view mod =
    div []
      [
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
        , ul [] (List.map (\description -> li [] [ text description ]) mod.entries)
      ]
