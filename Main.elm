import Browser
import Html exposing (Html, div, h1, h2, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (head, take, length, range, concat, map, member, reverse, drop)
import Time exposing (every)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)
import Random

-- https://ellie-app.com/3pwCCD723rTa1

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

type alias Game =
  { snake : Snake
  , bait : Bait
  , direction: Direction
  }

type alias Score = Int

type Status = NotPlaying
            | Playing Game
            | GameOver Score

init : () -> (Status, Cmd Msg)
init _ =
  ( NotPlaying
  , Cmd.none
  )

newGame : Game
newGame =
  { snake = [ {x = 10, y = 10}, {x = 11, y = 10}, {x = 12, y = 10}, {x = 13, y = 10} ]
  , bait = {x = 5, y = 5}
  , direction = Left
  }

gridSize : Int
gridSize = 15


-- UPDATE

type Msg
  = StartGame
  | Tick Time.Posix
  | Change Direction
  | NewBait Bait
  | DoNothing

update : Msg -> Status -> (Status, Cmd Msg)
update msg status =
  case msg of
    _ -> (status , Cmd.none)



-- VIEW

view : Status -> Html Msg
view status =
  case status of
    NotPlaying ->
      div [ class "not-playing" ] [ h1 [] [ text "Snake" ]
                                  , div [] [ button [ onClick StartGame ] [ text "Start Game" ] ]
                                  ]
    GameOver score ->
      div [ class "game-over" ] [ h1 [] [ text "Game Over" ]
                                , h2 [] [ text (String.fromInt score) ]
                                , div [] [ text "points" ]
                                , div [] [ button [ onClick StartGame ] [ text "Try Again" ] ]
                                ]
    Playing game ->
      div [ class "game" ] (drawGrid game)

drawGrid : Game -> List (Html Msg)
drawGrid model =
  let
    fields =
      map
        (\(coord, bool) ->
          div [ class (if bool == True then "snake" else "field") ] [])
        (renderGrid model.snake model.bait)
    drawLines = map (\l -> div [ class "line" ] l)
  in
  fields |> lines |> drawLines

lines fs = lines2 [] fs

lines2 =
  \l f ->
    case f of
      [] -> reverse l
      _ -> lines2 ((take gridSize f) :: l) (drop gridSize f)

renderGrid : Snake -> Bait -> List (Coordinate, Bool)
renderGrid snake bait =
  map
    (\c ->
      let
        filled = (member c snake) || c == bait
      in
      (c, filled))
    (emptyGrid 0 (gridSize - 1))

emptyGrid : Int -> Int -> List Coordinate
emptyGrid min max =
  concat (map
    (\(y, xs) -> map (\x -> {x = x, y = y}) xs)
    (map
      (\y -> (y, range min max))
      (range min max)))


-- SUBSCRIPTIONS

subscriptions : Status -> Sub Msg
subscriptions status =
  case status of
    Playing game ->
      Sub.batch
        [ every 100 Tick
        , onKeyDown movementKeyDecoder
        ]
    _ ->
      Sub.none

movementKeyDecoder : Decode.Decoder Msg
movementKeyDecoder =
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
      DoNothing
