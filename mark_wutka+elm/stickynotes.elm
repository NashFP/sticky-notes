import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (on, onClick)
import Mouse exposing (Position)
import List exposing(..)
import Json.Decode as Decode
import Debug exposing(log)

type alias Note =
    { id : String
    , x : Int
    , y : Int
    , startX : Int
    , startY : Int
    , color : String
    , text : String
    }

type alias Drag =
    { start : Position
    , current : Position
    }

type alias Model =
    { notes : List Note
    , nextId : Int
    , nextStart : Int
    , drag : Maybe Drag
    }

type Msg = NewNote
    | DragStart String Position
    | DragAt Position
    | DragEnd Position

init : (Model, Cmd Msg)
init =
    (Model []  1 0 Nothing, Cmd.none)

view : Model -> Html Msg
view model =
    div [ id "main" ]
        [ div [ id "icons" ] [
            div [ id "trash", class "trash" ] [],
            div [ id "add", class "add", onClick NewNote ] [] ],
          div [ id "board" ] 
            (renderNoteList 0 model.notes) ]

renderNoteList : Int -> List Note -> List (Html Msg)
renderNoteList currZ notes =
    case notes of
        [] -> []
        (x::xs) ->
            (renderNote x currZ) :: renderNoteList (currZ-1) xs

renderNote : Note -> Int -> Html Msg
renderNote note z =
    div [ id note.id, class ("note "++note.color),
          onMouseDown note.id,
          style [("top",(toString note.y)++"px"), 
                 ("left",(toString note.x)++"px"),
                 ("z-index",toString z) ]
        ]
        [ div [class "picker"] [],
          textarea [autocomplete False] [ text note.text ] ]

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing -> Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewNote -> (addNewNote model, Cmd.none)
        DragStart id xy ->
            let newModel = noteToTop id xy model
            in
                ({ newModel | drag = (Just (Drag xy xy)) }, Cmd.none)
        DragAt xy ->
            let newModel = moveNote xy model
            in
            ({ newModel | drag = Maybe.map (\{start} -> Drag start xy) model.drag },
             Cmd.none )
        DragEnd _ ->
            ({ model | drag = Nothing }, Cmd.none)

saveNoteXY : Note -> Note
saveNoteXY note =
    { note | startX = note.x, startY = note.y }

noteToTop id xy model =
    {model | notes = saveNoteXY (findNote id model.notes) :: (dropNote id model.notes) }

moveNote xy model =
    case model.drag of
        Nothing -> model
        Just {start,current} ->
            case model.notes of
                [] -> model
                (n::ns) -> {model | notes = (setNotePos xy start n) :: ns}

setNotePos xy start note =
    {note | x = note.startX + xy.x - start.x, y = note.startY + xy.y - start.y }

isNote id note = id == note.id
isNotNote id note = not (isNote id note)

findNote id notes =
    case notes of
        [] -> Debug.crash ("Note "++id++" not found")
        [x] -> if isNote id x then x else Debug.crash ("Note "++id++" not found")
        (x::xs) -> if isNote id x then x else findNote id xs

dropNote id notes = List.filter (isNotNote id) notes
    
addNewNote {notes, nextId, nextStart, drag} =
    let note =
        Note ("note_"++(toString nextId)) (50+nextStart) (50+nextStart) 0 0 "yellow" "hello"
    in
        Model (note::notes) (nextId+1) ((nextStart+1) % 50) drag

onMouseDown : String -> Attribute Msg
onMouseDown id =
    on "mousedown" (Decode.map (DragStart id) Mouse.position)

main =
    log "In main" (
    Html.program { init = init, view = view, update = update,
        subscriptions = subscriptions
    })
