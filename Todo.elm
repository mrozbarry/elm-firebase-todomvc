module Todo exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Json.Decode
import Json.Encode
import String
import Task exposing (Task)


-- Firebase modules

import Firebase
import Firebase.Errors exposing (Error)
import Firebase.Database
import Firebase.Database.Reference
import Firebase.Database.Snapshot
import Firebase.Database.Types exposing (Database, Reference, Snapshot)
import Firebase.Authentication
import Firebase.Authentication.Types exposing (Auth, User)


main : Program FirebaseConfig Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                let
                    todoRef : Reference
                    todoRef =
                        model.app
                            |> Firebase.Database.init
                            |> Firebase.Database.ref (Just "todo")
                in
                    Sub.batch [ Firebase.Database.Reference.on "value" todoRef GetEntries ]
        }



-- MODEL
-- The full application state of our todo app.


type alias Model =
    { app : Firebase.App
    , entries : List Entry
    , newTodoDescription : String
    , editingEntryId : Maybe String
    , visibility : String
    , signedIn : Bool
    }


type alias FirebaseConfig =
    { apiKey : String
    , authDomain : String
    , databaseURL : String
    , storageBucket : String
    , messagingSenderId : String
    }


type alias Entry =
    { id : String
    , description : String
    , completed : Bool
    }


initialModel : Firebase.App -> Model
initialModel app =
    { app = app
    , entries = []
    , newTodoDescription = ""
    , editingEntryId = Nothing
    , visibility = "All"
    , signedIn = False
    }


init : FirebaseConfig -> ( Model, Cmd Msg )
init config =
    let
        app : Firebase.App
        app =
            Firebase.init config

        auth : Auth
        auth =
            app
                |> Firebase.Authentication.init
    in
        ( initialModel app
        , Task.attempt FirebaseAuthResult (Firebase.Authentication.signInAnonymously auth)
        )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = GetEntries Snapshot
    | UpdateField String
    | EditingEntry (Maybe String)
    | UpdateEntry String String
    | Add
    | Delete String
    | DeleteComplete
    | Check String Bool
    | CheckAll Bool
    | ChangeVisibility String
    | FirebaseResult (Result Error ())
    | FirebaseAuthResult (Result Error User)
    | NoOp



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        rootRef : Reference
        rootRef =
            model.app
                |> Firebase.Database.init
                |> Firebase.Database.ref (Just "todo")
    in
        case msg of
            GetEntries snapshot ->
                let
                    entriesResult : Result String (List Entry)
                    entriesResult =
                        snapshot
                            |> Firebase.Database.Snapshot.value
                            |> Json.Decode.decodeValue decodeEntries

                    decodeEntries : Json.Decode.Decoder (List Entry)
                    decodeEntries =
                        Json.Decode.map
                            toEntryList
                            (Json.Decode.maybe (Json.Decode.keyValuePairs decodeEntry))

                    toEntryList : Maybe (List ( String, Entry )) -> List Entry
                    toEntryList maybeKeyValueEntries =
                        case maybeKeyValueEntries of
                            Just keyValueEntries ->
                                List.map
                                    (\( id, entry ) -> { entry | id = id })
                                    keyValueEntries

                            Nothing ->
                                []

                    decodeEntry : Json.Decode.Decoder Entry
                    decodeEntry =
                        Json.Decode.map2
                            (Entry "")
                            (Json.Decode.field "description" Json.Decode.string)
                            (Json.Decode.field "completed" Json.Decode.bool)
                in
                    case entriesResult of
                        Ok entries ->
                            ( { model | entries = entries }
                            , Cmd.none
                            )

                        Err decodeMessage ->
                            let
                                _ =
                                    Debug.log "Unable to decode todo list" decodeMessage
                            in
                                ( model
                                , Cmd.none
                                )

            UpdateField str ->
                ( { model | newTodoDescription = str }
                , Cmd.none
                )

            EditingEntry maybeId ->
                let
                    focusCmd : Cmd Msg
                    focusCmd =
                        case maybeId of
                            Just id ->
                                Task.attempt (\_ -> NoOp) (Dom.focus ("todo-" ++ id))

                            Nothing ->
                                Cmd.none
                in
                    ( { model | editingEntryId = maybeId }
                    , Cmd.batch [ focusCmd ]
                    )

            UpdateEntry id description ->
                let
                    writeUpdate : Task Error ()
                    writeUpdate =
                        rootRef
                            |> Firebase.Database.Reference.child id
                            |> Firebase.Database.Reference.child "description"
                            |> Firebase.Database.Reference.set (Json.Encode.string description)
                in
                    ( model
                    , Cmd.batch [ Task.attempt FirebaseResult writeUpdate ]
                    )

            Add ->
                let
                    writeTodo : Task Error ()
                    writeTodo =
                        ref
                            |> Firebase.Database.Reference.set encodeTodo

                    ref : Reference
                    ref =
                        Firebase.Database.Reference.push rootRef

                    encodeTodo : Json.Encode.Value
                    encodeTodo =
                        Json.Encode.object
                            [ ( "description", Json.Encode.string model.newTodoDescription )
                            , ( "completed", Json.Encode.bool False )
                            ]
                in
                    ( { model | newTodoDescription = "" }
                    , Cmd.batch [ Task.attempt FirebaseResult writeTodo ]
                    )

            Delete id ->
                let
                    removeTodo : Task Error ()
                    removeTodo =
                        rootRef
                            |> Firebase.Database.Reference.child id
                            |> Firebase.Database.Reference.set (Json.Encode.null)
                in
                    ( model
                    , Cmd.batch [ Task.attempt FirebaseResult removeTodo ]
                    )

            DeleteComplete ->
                let
                    removeCompleted : Task Error ()
                    removeCompleted =
                        rootRef
                            |> Firebase.Database.Reference.update setCompletedTodosToNull

                    setCompletedTodosToNull : Json.Encode.Value
                    setCompletedTodosToNull =
                        Json.Encode.object
                            (List.map (\todo -> ( todo.id, Json.Encode.null )) allCompletedTodos)

                    allCompletedTodos : List Entry
                    allCompletedTodos =
                        model.entries
                            |> List.filter (\entry -> entry.completed == True)
                in
                    ( model
                    , Cmd.batch [ Task.attempt FirebaseResult removeCompleted ]
                    )

            Check id isCompleted ->
                let
                    setComplete : Task Error ()
                    setComplete =
                        rootRef
                            |> Firebase.Database.Reference.child id
                            |> Firebase.Database.Reference.child "completed"
                            |> Firebase.Database.Reference.set (Json.Encode.bool isCompleted)
                in
                    ( model
                    , Cmd.batch [ Task.attempt FirebaseResult setComplete ]
                    )

            CheckAll isCompleted ->
                let
                    setAllCompleted : Task Error ()
                    setAllCompleted =
                        rootRef
                            |> Firebase.Database.Reference.update setEntriesCompleted

                    setEntriesCompleted : Json.Encode.Value
                    setEntriesCompleted =
                        Json.Encode.object
                            (List.map setEntryCompleted model.entries)

                    setEntryCompleted : Entry -> ( String, Json.Encode.Value )
                    setEntryCompleted entry =
                        ( entry.id
                        , Json.Encode.object
                            [ ( "completed", Json.Encode.bool isCompleted )
                            ]
                        )
                in
                    ( model
                    , Cmd.batch [ Task.attempt FirebaseResult setAllCompleted ]
                    )

            ChangeVisibility visibility ->
                ( { model | visibility = visibility }
                , Cmd.none
                )

            FirebaseResult (Ok _) ->
                ( model
                , Cmd.none
                )

            FirebaseResult (Err message) ->
                let
                    _ =
                        Debug.log "FirebaseResult.Err" message
                in
                    ( model
                    , Cmd.none
                    )

            FirebaseAuthResult (Ok _) ->
                ( { model | signedIn = True }
                , Cmd.none
                )

            FirebaseAuthResult (Err message) ->
                let
                    _ =
                        Debug.log "FirebaseAuthResult.Err" message
                in
                    ( model
                    , Cmd.none
                    )

            NoOp ->
                ( model
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style [ ( "visibility", "hidden" ) ]
        ]
        [ section
            [ class "todoapp" ]
            ((if model.signedIn then
                [ lazy viewInput model.newTodoDescription ]
              else
                []
             )
                ++ [ lazy viewEntries model
                   , lazy3 viewControls model.visibility model.signedIn model.entries
                   ]
            )
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : Model -> Html Msg
viewEntries model =
    let
        isVisible todo =
            case model.visibility of
                "Completed" ->
                    todo.completed

                "Active" ->
                    not todo.completed

                _ ->
                    True

        allCompleted =
            List.all .completed model.entries

        cssVisibility =
            if List.isEmpty model.entries then
                "hidden"
            else
                "visible"
    in
        section
            [ class "main"
            , style [ ( "visibility", cssVisibility ) ]
            ]
            [ input
                [ class "toggle-all"
                , type_ "checkbox"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                , disabled (not model.signedIn)
                ]
                []
            , label
                [ for "toggle-all" ]
                [ text "Mark all as complete" ]
            , Html.Keyed.ul [ class "todo-list" ] <|
                List.map (viewKeyedEntry model.signedIn model.editingEntryId) (List.filter isVisible model.entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Bool -> Maybe String -> Entry -> ( String, Html Msg )
viewKeyedEntry signedIn editingEntryId todo =
    let
        isEditing : Bool
        isEditing =
            (Just todo.id) == editingEntryId
    in
        ( todo.id
        , lazy3 viewEntry signedIn isEditing todo
        )


viewEntry : Bool -> Bool -> Entry -> Html Msg
viewEntry signedIn isEditing todo =
    case signedIn of
        True ->
            li
                [ classList [ ( "completed", todo.completed ), ( "editing", isEditing ) ] ]
                [ div
                    [ class "view" ]
                    [ input
                        [ class "toggle"
                        , type_ "checkbox"
                        , checked todo.completed
                        , onClick (Check todo.id (not todo.completed))
                        ]
                        []
                    , label
                        [ onDoubleClick (EditingEntry (Just todo.id)) ]
                        [ text todo.description ]
                    , button
                        [ class "destroy"
                        , onClick (Delete todo.id)
                        ]
                        []
                    ]
                , input
                    [ class "edit"
                    , value todo.description
                    , id ("todo-" ++ todo.id)
                    , onInput (UpdateEntry todo.id)
                    , onBlur (EditingEntry Nothing)
                    , onEnter (EditingEntry Nothing)
                    ]
                    []
                ]

        False ->
            li
                [ classList [ ( "completed", todo.completed ) ] ]
                [ div
                    [ class "view" ]
                    [ label
                        []
                        [ text todo.description ]
                    ]
                ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> Bool -> List Entry -> Html Msg
viewControls visibility signedIn entries =
    let
        entriesCompleted =
            List.length (List.filter .completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
        footer
            [ class "footer"
            , hidden (List.isEmpty entries)
            ]
            ([ lazy viewControlsCount entriesLeft
             , lazy viewControlsFilters visibility
             ]
                ++ (if signedIn then
                        [ lazy viewControlsClear entriesCompleted ]
                    else
                        []
                   )
            )


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"
            else
                " items"
    in
        span
            [ class "todo-count" ]
            [ strong [] [ text (toString entriesLeft) ]
            , text (item_ ++ " left")
            ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ toString entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/mrozbarry" ] [ text "Alex Barry" ]
            , text ", forked from work by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
