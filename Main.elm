import Html exposing (Html, div)
import Html.Attributes exposing (class, placeholder, href)
import Html.Events exposing (onInput, onClick, onSubmit)
import List exposing (head, take, length, range, concat, map, member)
import Time exposing (Time, every, millisecond)


main =
  Html.program
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

init : (Model, Cmd Msg)
init =
  ( { snake = [ { x = 0, y = 0}, { x = 1, y = 0}, {x = 2, y = 0}, {x = 3, y = 0} ]
    , bait = {x = 5, y = -5}
    , direction = Left
    }
  , Cmd.none
  )


-- UPDATE

type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( move model
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
          { oldHead | y = oldHead.y + 1 }
        Down ->
          { oldHead | y = oldHead.y - 1 }
        Right ->
          { oldHead | x = oldHead.x + 1 }
        Left ->
          { oldHead | x = oldHead.x - 1 }
  in
  { model | snake = newHead :: take (length model.snake - 1) model.snake}

-- VIEW

view : Model -> Html Msg
view model =
  div [ class "wrapper"]
    [
    ]

renderGrid : Snake -> Bait -> List (Coordinate, Bool)
renderGrid snake bait =
  map
    (\c ->
      let
        filled = (member c snake) || c == bait
      in
      (c, filled))
    (emptyGrid -5 5)

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
  every (333 * millisecond) Tick

