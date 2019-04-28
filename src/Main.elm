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


type Visibility
    = All
    | Active
    | Completed


type alias Model =
    { uid : Int
    , description : String
    , entries : List Entry
    , visible : Visibility
    }


type Msg
    = SetDescription String
    | AddEntry
    | ToggleEntry Int Bool
    | ToggleEntries Bool
    | DeleteEntry Int
    | RemoveCompletedEntries
    | SetVisible Visibility


init : () -> ( Model, Cmd Msg )
init _ =
    ( { description = "", entries = [], uid = 0, visible = All }
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
                    , entries = model.entries ++ [ createEntry model.uid cleanDescription ]
                    , uid = model.uid + 1
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

        SetVisible visible ->
            ( { model | visible = visible }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view mod =
    div []
        [ viewPrompt mod.description
        , viewBody mod.visible mod.entries
        ]


viewPrompt : String -> Html Msg
viewPrompt description =
    Html.form [ Events.onSubmit AddEntry ]
        [ input
            [ type_ "text"
            , autofocus True
            , placeholder "What needs to be done?"
            , value description
            , Events.onInput SetDescription
            ]
            []
        ]


viewBody : Visibility -> List Entry -> Html Msg
viewBody visible entries =
    if List.isEmpty entries then
        text ""

    else
        div []
            [ label []
                [ input
                    [ type_ "checkbox"
                    , checked (List.all .completed entries)
                    , Events.onCheck ToggleEntries
                    ]
                    []
                , text "Mark all as completed"
                ]
            , ul []
                (List.map (\entry -> li [] [ viewEntry entry ])
                    (filterVisibility visible entries)
                )
            , div [] [ text (calcInCompleteTasks entries) ]
            , viewVisibilityFilters visible
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


viewVisibilityFilters : Visibility -> Html Msg
viewVisibilityFilters selected =
    let
        viewVisibilityFilter name url current sel =
            if current == sel then
                span [] [ text name ]

            else
                a [ href url, Events.onClick (SetVisible current) ] [ text name ]
    in
    div []
        [ viewVisibilityFilter "All" "#" All selected
        , text " "
        , viewVisibilityFilter "Active" "#active" Active selected
        , text " "
        , viewVisibilityFilter "Completed" "#completed" Completed selected
        ]



-- HELPERS


calcInCompleteTasks : List Entry -> String
calcInCompleteTasks entries =
    let
        n =
            entries
                |> List.filter (\en -> en.completed /= True)
                |> List.length
    in
    if n > 1 then
        String.fromInt n ++ " tasks left"

    else
        String.fromInt n ++ " task left"


filterVisibility : Visibility -> List Entry -> List Entry
filterVisibility visible entries =
    case visible of
        All ->
            entries

        Active ->
            List.filter (not << .completed) entries

        Completed ->
            List.filter .completed entries
