import Browser
import Html exposing (Html, div, h1, h2, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List exposing (head, take, length, range, concat, map, member, reverse, drop)
import Time exposing (every)
import Json.Decode as Decode
import Browser.Events exposing (onKeyDown)
import Random


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
  { snake = [ { x = 10, y = 10}, { x = 11, y = 10}, {x = 12, y = 10}, {x = 13, y = 10} ]
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

update : Msg -> Status -> (Status, Cmd Msg)
update msg status =
  case msg of
    StartGame ->
      ( Playing newGame
      , Cmd.none
      )
    Tick _ ->
      case status of
        Playing game ->
          ( status |> move |> preventDeath |> preventEscape
          , generateNewBait game
          )
        _ -> ( NotPlaying , Cmd.none)
    Change direction ->
      case status of
        Playing game ->
          ( Playing (changeDirection direction game)
          , Cmd.none
          )
        _ -> ( NotPlaying , Cmd.none)
    NewBait bait ->
      case status of
        Playing game ->
          ( Playing { game | bait = bait }
          , Cmd.none
          )
        other -> ( other , Cmd.none)

move : Status -> Status
move status =
  case status of
    Playing game ->
      let
        oldHead =
          case game.snake of
            [] -> {x = 0, y = 0} -- shouldn't happen
            (x :: xs) -> x

        newHead =
          case game.direction of
            Up ->
              { oldHead | y = oldHead.y - 1 }
            Down ->
              { oldHead | y = oldHead.y + 1 }
            Right ->
              { oldHead | x = oldHead.x + 1 }
            Left ->
              { oldHead | x = oldHead.x - 1 }

        movement = if (member game.bait game.snake) then 0 else 1
      in
      Playing { game | snake = newHead :: take (length game.snake - movement) game.snake }
    _ -> status

preventDeath : Status -> Status
preventDeath status =
  case status of
    Playing game ->
      case game.snake of
        (snakeHead :: snakeTail) ->
          if member snakeHead snakeTail
          then GameOver (length game.snake)
          else status
        _ -> status
    _ -> status

preventEscape : Status -> Status
preventEscape status =
  case status of
    Playing game ->
      case game.snake of
        (snakeHead :: snakeTail) ->
          if member snakeHead (emptyGrid 0 (gridSize - 1))
          then status
          else GameOver (length game.snake)
        _ -> status
    _ -> status

generateNewBait : Game -> Cmd Msg
generateNewBait game =
  if (member game.bait game.snake)
  then Random.generate NewBait randomBait
  else Cmd.none

randomBait : Random.Generator Bait
randomBait =
  Random.map2
    (\x y -> {x = x, y = y})
    (Random.int 0 (gridSize - 1))
    (Random.int 0 (gridSize - 1))

changeDirection : Direction-> Game -> Game
changeDirection direction game =
  case direction of
    Up -> if game.direction == Down then game else { game | direction = Up }
    Down -> if game.direction == Up then game else { game | direction = Down }
    Right -> if game.direction == Left then game else { game | direction = Right }
    Left -> if game.direction == Right then game else { game | direction = Left }


-- VIEW

view : Status -> Html Msg
view status =
  case status of
    NotPlaying ->
      div [ class "not-playing" ] [ h1 [] [ text "Snake" ]
                                  , button [ onClick StartGame] [ text "Start Game" ]
                                  ]
    Playing game ->
      div [ class "game" ] (drawGrid game)
    GameOver score ->
      div [ class "game-over" ] [ h1 [] [ text "Game Over" ]
                                , h2 [] [text (String.fromInt score) ]
                                , text "points"
                                , div [] [ button [ onClick StartGame] [ text "Try Again" ] ]
                                ]

drawGrid : Game -> List (Html Msg)
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
      Change Left
