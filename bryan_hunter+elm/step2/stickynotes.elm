module StickyNotes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Decode
import Mouse exposing (Position)

{-- 
    Live reload
    elm-live stickynotes.elm --open --warn --output=stickynotes.js 
--}

-- MODEL
type alias Model =
    { position : Position
    , drag : Maybe Drag
    }

type alias Drag =
    { start : Position
    , current : Position
    }

type CardColor
  = Yellow
  | Blue
  | Green
  | Pink


-- UPDATE
type Msg
    = DragStart Position
    | DragAt Position
    | DragEnd Position

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  ( updateHelp msg model, Cmd.none )

updateHelp : Msg -> Model -> Model
updateHelp msg ({position, drag} as model) =
  case msg of
    DragStart xy ->
      Model position (Just (Drag xy xy))

    DragAt xy ->
      Model position (Maybe.map (\{start} -> Drag start xy) drag)

    DragEnd _ ->
      Model (getPosition model) Nothing

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

-- VIEW
(=>) : a -> b -> ( a, b )
(=>) = (,)

viewCard : Model -> Html Msg
viewCard model =
  let
    realPosition =
      getPosition model
  in
    div [class "note", class "yellow", onMouseDown,
          style [ "left" => px realPosition.x
                , "top" => px realPosition.y
                ]
        ]
        [ textarea [] [ ] ]

viewIcons : Model -> Html Msg
viewIcons model =
  div [ id "icons" ] [ div [ class "trash"] [], div [ class "add" ] [] ]

view : Model -> Html Msg
view model =
  div [ id "board" ] [ viewIcons model, viewCard model ]
  
px : Int -> String
px number =
  toString number ++ "px"

getPosition : Model -> Position
getPosition {position, drag} =
  case drag of
    Nothing ->
      position

    Just {start,current} ->
      Position
        (position.x + current.x - start.x)
        (position.y + current.y - start.y)

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Decode.map DragStart Mouse.position)


init : ( Model, Cmd Msg )
init =
  ( Model (Position 200 200) Nothing, Cmd.none )

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
