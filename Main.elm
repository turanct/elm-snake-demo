import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import List exposing (head, take, length, range, concat, map, member, reverse, drop)
import Time exposing (every)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Coordinate = { x: Int, y: Int }

type alias Snake = List Coordinate

type alias Bait = Coordinate

type Direction = Up
               | Down
               | Left
               | Right

type alias Model =
  { snake : Snake
  , bait : Bait
  , direction: Direction
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { snake = [ { x = 0, y = 0}, { x = 1, y = 0}, {x = 2, y = 0}, {x = 3, y = 0} ]
    , bait = {x = 5, y = 5}
    , direction = Left
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time.Posix
  | Change Direction

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick _ ->
      ( move model
      , Cmd.none
      )
    Change direction ->
      ( { model | direction = direction }
      , Cmd.none
      )

move : Model -> Model
move model =
  let
    oldHead =
      case model.snake of
        [] -> {x = 0, y = 0} -- shouldn't happen
        (x :: xs) -> x

    newHead =
      case model.direction of
        Up ->
          { oldHead | y = oldHead.y - 1 }
        Down ->
          { oldHead | y = oldHead.y + 1 }
        Right ->
          { oldHead | x = oldHead.x + 1 }
        Left ->
          { oldHead | x = oldHead.x - 1 }
  in
  { model | snake = newHead :: take (length model.snake - 1) model.snake}


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "wrapper" ] (drawGrid model)

drawGrid : Model -> List (Html Msg)
drawGrid model =
  let
    fields =
      map
        (\(coord, bool) ->
          div [ class (if bool == True then "snake" else "field") ] [])
        (renderGrid model.snake model.bait)
    drawLines = map (\l -> div [ class "line"] l)
  in
  fields |> lines |> drawLines

lines fs = lines2 [] fs

lines2 =
  \l f ->
    case f of
      [] -> reverse l
      _ -> lines2 ((take 20 f) :: l) (drop 20 f)

renderGrid : Snake -> Bait -> List (Coordinate, Bool)
renderGrid snake bait =
  map
    (\c ->
      let
        filled = (member c snake) || c == bait
      in
      (c, filled))
    (emptyGrid 0 19)

emptyGrid : Int -> Int -> List Coordinate
emptyGrid min max =
  concat (map
    (\(y, xs) -> map (\x -> {x = x, y = y}) xs)
    (map
      (\y -> (y, range min max))
      (range min max)))


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ every 150 Tick
    , onKeyDown keyDecoder
    ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowUp" ->
      Change Up
    "ArrowDown" ->
      Change Down
    "ArrowRight" ->
      Change Right
    "ArrowLeft" ->
      Change Left
    _ ->
      Change Left
