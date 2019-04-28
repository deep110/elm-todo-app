module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- MODEL


type alias Entry =
    { id : Int, name : String, completed : Bool }


type alias Model =
    { description : String, entries : List Entry }


type Msg
    = SetDescription String
    | AddEntry
    | ToggleEntry Int Bool


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
                ( { model
                    | description = ""
                    , entries = model.entries ++ [ createEntry (List.length model.entries) cleanDescription ]
                  }
                , Cmd.none
                )

        ToggleEntry entryId completed ->
            let
                updateEntry entry =
                    if entry.id == entryId then
                        { entry | completed = True }

                    else
                        entry
            in
            ( { model | entries = List.map updateEntry model.entries }, Cmd.none )


createEntry : Int -> String -> Entry
createEntry id name =
    { id = id, name = name, completed = False }



-- VIEW


view : Model -> Html Msg
view mod =
    div []
        [ Html.form [ Events.onSubmit AddEntry ]
            [ input
                [ type_ "text"
                , autofocus True
                , placeholder "What needs to be done?"
                , value mod.description
                , Events.onInput SetDescription
                ]
                []
            ]
        , ul [] (List.map (\entry -> li [] [ viewEntry entry ]) mod.entries)
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    div []
        [ input
            [ type_ "checkbox"
            , checked entry.completed
            , Events.onCheck (ToggleEntry entry.id)
            ]
            []
        , span
            [ classList [ ( "line-through", entry.completed ) ] ]
            [ text entry.name ]
        ]
