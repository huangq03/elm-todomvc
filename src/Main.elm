port module Main exposing (..)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

import Browser
import Browser.Dom as Dom
import Http
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Task
import Time


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )



-- MODEL


-- The full application state of our todo app.
type alias Model =
    { entries : List (Todo, Bool)
    , field : String
    , visibility : String
    }


type alias Todo =
    { content : String
    , deadline : Int
    , done : Bool
    , todoLogId : Int
    , todoId : Int
    , repeatType: String
    , status: String
    }


emptyModel : Model
emptyModel =
    { entries = []
    , visibility = "All"
    , field = ""
    }


newTodo : String -> Todo
newTodo desc =
    { content = desc
    , deadline = 0
    , done = False
    , todoLogId = 0
    , todoId = 0
    , repeatType = ""
    , status = "Ongoing"
    }

getTodos : Cmd Msg
getTodos =
  Http.get
    { url = "/api/todolog"
    , expect = Http.expectJson TodosReceived todosDecoder
    }

todosDecoder : Json.Decoder (List Todo)
todosDecoder = Json.list todoDecoder

todoDecoder : Json.Decoder Todo
todoDecoder =
  Json.map7 Todo
    (Json.field "content" Json.string)
    (Json.field "deadline" Json.int)
    (Json.field "done" Json.bool)
    (Json.field "todoLogId" Json.int)
    (Json.field "todoId" Json.int)
    (Json.field "repeatType" Json.string)
    (Json.field "status" Json.string)

init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
  ( Maybe.withDefault emptyModel maybeModel
  , getTodos
  )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | UpdateField String
    | EditingTodo Int Bool
    | UpdateTodo Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | TodosReceived (Result Http.Error (List Todo))



-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TodosReceived result ->
          case result of
            Ok todos ->
              let
                  todo2Entry t = (t, False)
              in
              ( { model | entries = List.map todo2Entry todos }, Cmd.none )
            Err _ ->
              (  model, Cmd.none )
        Add ->
            ( { model
                | field = ""
                , entries =
                    if String.isEmpty model.field then
                        model.entries
                    else
                        model.entries ++ [ (newTodo model.field, False) ]
              }
            , Cmd.none
            )

        UpdateField str ->
            ( { model | field = str }
            , Cmd.none
            )

        EditingTodo id isEditing ->
            let
                updateTodo (t, e) =
                    if t.todoLogId == id then
                        ( t, isEditing )
                    else
                        ( t, e )

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( { model | entries = List.map updateTodo model.entries }
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateTodo id task ->
            let
                updateTodo (t, e) =
                    if t.todoLogId == id then
                        ( { t | content = task }, e )
                    else
                        ( t, e )
            in
            ( { model | entries = List.map updateTodo model.entries }
            , Cmd.none
            )

        Delete id ->
            ( { model | entries = List.filter (checkTodoById id) model.entries }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | entries = List.filter (checkTodoByDone False) model.entries }
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateTodo (t, e) =
                    if t.todoLogId == id then
                        (t, isCompleted)
                    else
                        (t, e)
            in
            ( { model | entries = List.map updateTodo model.entries }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateTodo (t, _) =
                    (t, isCompleted)
            in
            ( { model | entries = List.map updateTodo model.entries }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ Html.section
            [ class "todoapp" ]
            [ lazy viewInput model.field
            , lazy2 viewEntries model.visibility model.entries
            , lazy2 viewControls model.visibility model.entries
            ]
        , infoFooter
        ]


viewInput : String -> Html.Html Msg
viewInput task =
    Html.header
        [ class "header" ]
        [ Html.h1 [] [ Html.text "todos" ]
        , Html.input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> List (Todo, Bool) -> Html.Html Msg
viewEntries visibility entries =
    let
        isVisible (todo, _) =
            case visibility of
                "Completed" ->
                    todo.done

                "Active" ->
                    not todo.done

                _ ->
                    True

        allCompleted =
            List.all (checkTodoByDone True) entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"
            else
                "visible"
    in
        Html.section
            [ class "main"
            , style "visibility" cssVisibility
            ]
            [ Html.input
                [ class "toggle-all"
                , type_ "checkbox"
                , name "toggle"
                , checked allCompleted
                , onClick (CheckAll (not allCompleted))
                ]
                []
            , Html.label
                [ for "toggle-all" ]
                [ Html.text "Mark all as complete" ]
            , Keyed.ul [ class "todo-list" ] <|
                List.map viewKeyedTodo (List.filter isVisible entries)
            ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedTodo : (Todo, Bool) -> ( String, Html.Html Msg )
viewKeyedTodo (todo, editing) =
    ( String.fromInt todo.todoLogId, lazy viewTodo (todo, editing ))


viewTodo : (Todo, Bool) -> Html.Html Msg
viewTodo (todo, editing) =
    Html.li
        [ classList [ ( "done", todo.done ), ( "editing", editing ) ] ]
        [ Html.div
            [ class "view" ]
            [ Html.input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.done
                , onClick (Check todo.todoLogId (not todo.done))
                ]
                []
            , Html.label
                [ onDoubleClick (EditingTodo todo.todoLogId True) ]
                [ Html.text todo.content ]
            , Html.button
                [ class "destroy"
                , onClick (Delete todo.todoLogId)
                ]
                []
            ]
        , Html.input
            [ class "edit"
            , value todo.content
            , name "title"
            , id ("todo-" ++ String.fromInt todo.todoLogId)
            , onInput (UpdateTodo todo.todoLogId)
            , onBlur (EditingTodo todo.todoLogId False)
            , onEnter (EditingTodo todo.todoLogId False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List (Todo, Bool) -> Html.Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter (checkTodoByDone True) entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
        Html.footer
            [ class "footer"
            , hidden (List.isEmpty entries)
            ]
            [ lazy viewControlsCount entriesLeft
            , lazy viewControlsFilters visibility
            , lazy viewControlsClear entriesCompleted
            ]


viewControlsCount : Int -> Html.Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"
            else
                " items"
    in
        Html.span
            [ class "todo-count" ]
            [ Html.strong [] [ Html.text (String.fromInt entriesLeft) ]
            , Html.text (item_ ++ " left")
            ]


viewControlsFilters : String -> Html.Html Msg
viewControlsFilters visibility =
    Html.ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , Html.text " "
        , visibilitySwap "#/active" "Active" visibility
        , Html.text " "
        , visibilitySwap "#/done" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html.Html Msg
visibilitySwap uri visibility actualVisibility =
    Html.li
        [ onClick (ChangeVisibility visibility) ]
        [ Html.a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ Html.text visibility ]
        ]


viewControlsClear : Int -> Html.Html Msg
viewControlsClear entriesCompleted =
    Html.button
        [ class "clear-done"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ Html.text ("Clear done (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html.Html msg
infoFooter =
    Html.footer [ class "info" ]
        [ Html.p [] [ Html.text "Double-click to edit a todo" ]
        , Html.p []
            [ Html.text "Written by "
            , Html.a [ href "https://github.com/evancz" ] [ Html.text "Evan Czaplicki" ]
            ]
        , Html.p []
            [ Html.text "Part of "
            , Html.a [ href "http://todomvc.com" ] [ Html.text "TodoMVC" ]
            ]
        ]

checkTodoById : Int -> (Todo, Bool) -> Bool
checkTodoById id (t ,e) =
  if t.todoLogId == id then
    True
  else
    False

checkTodoByDone : Bool -> (Todo, Bool) -> Bool
checkTodoByDone done (t, e) =
  if t.done == done then
    True
  else
    False
