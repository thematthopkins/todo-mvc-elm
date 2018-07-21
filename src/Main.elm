port module Main exposing (init, update, view)

import Html exposing (..)
import Html.Keyed
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)
import Dom
import Task


---- PORTS ----
-- port for sending strings out to JavaScript


port persistList : PersistedItems -> Cmd msg


persistCmd : Model -> Cmd msg
persistCmd model =
    persistList (modelToPersistedItems model)


persistedItemsToModel : PersistedItems -> Route -> Model
persistedItemsToModel persistedItems route =
    { pendingItem = ""
    , items =
        List.map
            (\item ->
                { id = item.id
                , checked = item.checked
                , label = item.label
                , newLabel = item.label
                , editing = False
                }
            )
            persistedItems.items
    , route = route
    , nextID = 0
    }


modelToPersistedItems : Model -> PersistedItems
modelToPersistedItems model =
    { nextID = model.nextID
    , items =
        List.map
            (\i ->
                { id = i.id
                , label = i.label
                , checked = i.checked
                }
            )
            model.items
    }



---- ROUTES ----


type Route
    = TodoListRoute ItemsFilter
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ UrlParser.map (TodoListRoute AllItems) top
        , UrlParser.map (TodoListRoute CompletedItems) (UrlParser.s "completed")
        , UrlParser.map (TodoListRoute ActiveItems) (UrlParser.s "active")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



---- MODEL ----


type ItemsFilter
    = AllItems
    | CompletedItems
    | ActiveItems


type alias ItemLabel =
    String


type alias Item =
    { id : ItemIndex
    , checked : Bool
    , label : String
    , newLabel : String
    , editing : Bool
    }


type alias ItemIndex =
    Int


type alias Model =
    { pendingItem : String
    , nextID : ItemIndex
    , items : List Item
    , route : Route
    }


type alias PersistedItem =
    { id : ItemIndex
    , label : String
    , checked : Bool
    }


type alias PersistedItems =
    { nextID : ItemIndex
    , items : List PersistedItem
    }


type alias Flags =
    { items : PersistedItems
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    ( persistedItemsToModel flags.items (parseLocation location)
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | PendingItemChanged ItemLabel
    | ItemChecked ItemIndex Bool
    | CheckAll Bool
    | AddPendingItem
    | ClearCompleted
    | DestroyItem ItemIndex
    | EditItem ItemIndex
    | ItemLabelChanged ItemIndex ItemLabel
    | ItemLabelChangeCancelled ItemIndex
    | ItemLabelChangeSaved ItemIndex
    | LocationChanged Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PendingItemChanged itemLabel ->
            ( { model | pendingItem = itemLabel }, Cmd.none )

        AddPendingItem ->
            let
                trimmed =
                    String.trim model.pendingItem
            in
                if trimmed == "" then
                    ( model, Cmd.none )
                else
                    let
                        updated =
                            { model
                                | items =
                                    List.append model.items
                                        [ { label = trimmed
                                          , checked = False
                                          , id = model.nextID
                                          , editing = False
                                          , newLabel = trimmed
                                          }
                                        ]
                                , pendingItem = ""
                                , nextID = model.nextID + 1
                            }
                    in
                        ( updated, persistCmd updated )

        ItemChecked index checked ->
            let
                updated =
                    { model
                        | items =
                            List.map
                                (\item ->
                                    { item
                                        | checked =
                                            if item.id == index then
                                                checked
                                            else
                                                item.checked
                                    }
                                )
                                model.items
                    }
            in
                ( updated
                , persistCmd updated
                )

        CheckAll isChecked ->
            let
                updated =
                    { model
                        | items =
                            List.map
                                (\item -> { item | checked = isChecked })
                                model.items
                    }
            in
                ( updated
                , persistCmd updated
                )

        ClearCompleted ->
            let
                updated =
                    { model
                        | items =
                            List.filter
                                (\item -> not item.checked)
                                model.items
                    }
            in
                ( updated
                , persistCmd updated
                )

        DestroyItem id ->
            let
                updated =
                    { model
                        | items =
                            List.filterMap
                                (\item ->
                                    if item.id /= id then
                                        Just item
                                    else
                                        Nothing
                                )
                                model.items
                    }
            in
                ( updated
                , persistCmd updated
                )

        EditItem id ->
            ( { model
                | items =
                    List.map
                        (\item ->
                            if item.id == id then
                                { item | editing = True }
                            else
                                item
                        )
                        model.items
              }
            , Dom.focus ("edit_" ++ (toString id)) |> Task.attempt (\r -> NoOp)
            )

        LocationChanged location ->
            ( { model | route = parseLocation location }, Cmd.none )

        ItemLabelChanged id label ->
            ( { model
                | items =
                    List.map
                        (\item ->
                            if item.id == id then
                                { item | newLabel = label }
                            else
                                item
                        )
                        model.items
              }
            , Cmd.none
            )

        ItemLabelChangeCancelled id ->
            ( { model
                | items =
                    List.map
                        (\item ->
                            if item.id == id then
                                { item | newLabel = item.label, editing = False }
                            else
                                item
                        )
                        model.items
              }
            , Cmd.none
            )

        ItemLabelChangeSaved id ->
            let
                updated =
                    { model
                        | items =
                            List.filterMap
                                (\item ->
                                    if item.id == id then
                                        let
                                            trimmed =
                                                String.trim item.newLabel
                                        in
                                            if trimmed == "" then
                                                Nothing
                                            else
                                                Just { item | label = trimmed, editing = False }
                                    else
                                        Just item
                                )
                                model.items
                    }
            in
                ( updated
                , persistCmd updated
                )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


appFooterView : Html msg
appFooterView =
    footer [ class "info" ]
        [ p []
            [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/addyosmani" ]
                [ text "Addy Osmani" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ]
                [ text "TodoMVC" ]
            ]
        ]


addItemView : String -> Html Msg
addItemView pendingItem =
    header [ class "header" ]
        [ h1 []
            [ text "todos" ]
        , Html.form [ onSubmit AddPendingItem ]
            [ input
                [ attribute "autofocus" "", class "new-todo", placeholder "What needs to be done?", onInput PendingItemChanged, value pendingItem ]
                []
            ]
        ]


itemView : Item -> Html Msg
itemView item =
    li
        [ classList
            [ ( "completed", item.checked )
            , ( "editing", item.editing )
            ]
        ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked item.checked, onCheck (ItemChecked item.id) ]
                []
            , label [ Html.Events.onDoubleClick (EditItem item.id) ]
                [ text item.label ]
            , button [ class "destroy", onClick (DestroyItem item.id) ]
                []
            ]
        , Html.form [ onSubmit (ItemLabelChangeSaved item.id) ]
            [ input
                [ class "edit", id ("edit_" ++ (toString item.id)), value item.newLabel, onInput (ItemLabelChanged item.id), onBlur (ItemLabelChangeSaved item.id) ]
                []
            ]
        ]


itemsView : List Item -> Html Msg
itemsView items =
    section [ class "main", classList [ ( "hidden", List.isEmpty items ) ] ]
        [ input [ class "toggle-all", id "toggle-all", type_ "checkbox", checked (List.all (\i -> i.checked) items), onCheck CheckAll ]
            []
        , label [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Html.Keyed.ul [ class "todo-list" ]
            (List.map
                (\item -> ( toString item.id, itemView item ))
                items
            )
        ]


itemsFooterView : List Item -> Route -> Html Msg
itemsFooterView items route =
    let
        itemCount =
            List.length items

        hasCompletedItems =
            List.any (\item -> item.checked) items
    in
        footer [ class "footer", classList [ ( "hidden", List.isEmpty items ) ] ]
            [ span [ class "todo-count" ]
                [ text (toString itemCount ++ " items left") ]
            , ul [ class "filters" ]
                [ li []
                    [ a [ href "#/", classList [ ( "selected", route == TodoListRoute AllItems ) ] ]
                        [ text "All" ]
                    ]
                , li []
                    [ a [ href "#/active", classList [ ( "selected", route == TodoListRoute ActiveItems ) ] ]
                        [ text "Active" ]
                    ]
                , li []
                    [ a [ href "#/completed", classList [ ( "selected", route == TodoListRoute CompletedItems ) ] ]
                        [ text "Completed" ]
                    ]
                ]
            , button
                [ class "clear-completed"
                , onClick ClearCompleted
                , classList
                    [ ( "hidden", not hasCompletedItems ) ]
                ]
                [ text "Clear completed" ]
            ]


view : Model -> Html Msg
view model =
    let
        visibleItems =
            case model.route of
                TodoListRoute AllItems ->
                    model.items

                TodoListRoute CompletedItems ->
                    List.filter (\item -> item.checked) model.items

                TodoListRoute ActiveItems ->
                    List.filter (\item -> not item.checked) model.items

                NotFoundRoute ->
                    []
    in
        body []
            [ section [ class "todoapp" ]
                [ addItemView model.pendingItem
                , itemsView visibleItems
                , itemsFooterView model.items model.route
                ]
            , appFooterView
            ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChanged
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
