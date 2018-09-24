import Browser
import Html exposing (Html, div, h1, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- https://ellie-app.com/3rgFRnxkWFka1

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL
type alias Counter = Int

init : () -> (Counter, Cmd Msg)
init _ =
  ( 0
  , Cmd.none
  )


-- UPDATE
type Msg
  = Increase Int
  | Decrease Int

update : Msg -> Counter -> (Counter, Cmd Msg)
update msg counter =
  case msg of
    Increase number ->
      ( counter + number, Cmd.none)
    Decrease number ->
      ( counter - number , Cmd.none)


-- VIEW
view : Counter -> Html Msg
view counter =
  div [ class "counter" ] [ h1 [] [ text (String.fromInt counter) ]
                          , div [] [ button [ onClick (Decrease 10) ] [ text "- 10" ] ]
                          , div [] [ button [ onClick (Decrease 1) ] [ text "- 1" ] ]
                          , div [] [ button [ onClick (Increase 1) ] [ text "+ 1" ] ]
                          , div [] [ button [ onClick (Increase 10) ] [ text "+ 10" ] ]
                          ]


-- SUBSCRIPTIONS

subscriptions : Counter -> Sub Msg
subscriptions counter = Sub.none
