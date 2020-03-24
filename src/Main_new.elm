module Main_new exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Ready String
  | Success (List String)


init : () -> (Model, Cmd Msg)
init _ =
  (Ready "", Cmd.none)



-- UPDATE


type Msg
  = Fetch String
  | Change String
  | GotCountyList (Result Http.Error (List String))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      (Ready newContent, Cmd.none)

    Fetch zip ->
      (Loading, getCounties zip)

    GotCountyList result ->
      case result of
        Ok payload ->
          (Success payload, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "View Zips" ]
    , viewCounties model
    ]


viewCounties : Model -> Html Msg
viewCounties model =
  case model of
    Failure ->
      div []
        [ text "I could not load counties for some reason. "
        , button [ onClick (Change ""), style "display" "block" ] [ text "Reset" ]
        ]

    Ready str ->
      div []
        [ text "Enter ZIP code"
        , div [] [ renderInput str ]
        ]

    Loading ->
      text "Loading..."

    Success lstr ->
      div []
        [ button [ onClick (Change ""), style "display" "block" ] [ text "Reset" ]
        , div [] [ renderList lstr ]
        ]



-- HTTP


getCounties : String -> Cmd Msg
getCounties zip =
  Http.get
    { url = "http://localhost:5000/api/counties?zip=" ++ zip
    , expect = Http.expectJson GotCountyList countyDecoder
    }


countyDecoder : Decoder (List (String))
countyDecoder =
  field "zip" (Json.Decode.list string)

renderList : List String -> Html Msg
renderList lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []

renderInput : String -> Html Msg
renderInput str =
  div []
    [ input [ placeholder "Enter Zip Code", value str, onInput (\a -> Change a) ] []
    , div []
      [ button [ onClick (Fetch str) ] [ text "Submit" ] ]
    ]
