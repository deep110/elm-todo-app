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
    | ToggleEntries Bool
    | DeleteEntry Int
    | RemoveCompletedEntries


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

                createEntry id name =
                    { id = id, name = name, completed = False }
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
                        { entry | completed = completed }

                    else
                        entry
            in
            ( { model | entries = List.map updateEntry model.entries }, Cmd.none )

        ToggleEntries done ->
            ( { model
                | entries =
                    List.map (\entry -> { entry | completed = done }) model.entries
              }
            , Cmd.none
            )

        DeleteEntry entryId ->
            ( { model
                | entries =
                    List.filter (\entry -> entry.id /= entryId) model.entries
              }
            , Cmd.none
            )

        RemoveCompletedEntries ->
            ( { model
                | entries =
                    List.filter (\entry -> entry.completed /= True) model.entries
              }
            , Cmd.none
            )



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
        , label []
            [ input
                [ type_ "checkbox"
                , checked (List.all .completed mod.entries)
                , Events.onCheck ToggleEntries
                ]
                []
            , text "Mark all as completed"
            ]
        , ul [] (List.map (\entry -> li [] [ viewEntry entry ]) mod.entries)
        , button
            [ type_ "button"
            , Events.onClick RemoveCompletedEntries
            ]
            [ text "Clear completed" ]
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    div [ class "hover-target" ]
        [ input
            [ type_ "checkbox"
            , checked entry.completed
            , Events.onCheck (ToggleEntry entry.id)
            ]
            []
        , span
            [ classList [ ( "line-through", entry.completed ) ] ]
            [ text entry.name ]
        , button
            [ type_ "button"
            , class "ml-1 visible-on-hover"
            , Events.onClick (DeleteEntry entry.id)
            ]
            [ text "x" ]
        ]
