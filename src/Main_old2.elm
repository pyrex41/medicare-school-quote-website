module Main exposing (..)

-- Press a button to send a GET request for random cat GIFs.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/json.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, succeed, list, string, map2)
import Json.Decode.Pipeline


-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type Model
  = Failure Http.Error
  | Loading
  | ShowUser User
  | ShowCounties ( List ( String ))
  | Zip5 String

type alias User = { username : String, password : String }

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getUserData)



-- UPDATE


type Msg
  = MorePlease
  | GotUser (Result Http.Error User)
  | GotCountyList (Result Http.Error (List ( String ) ))
  | SaveZip String
  | SubmitZip


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getUserData)

    GotUser result ->
      case result of
        Ok payload ->
          (ShowUser payload, Cmd.none)

        Err payload ->
          (Failure payload, Cmd.none)

    GotCountyList result ->
      case result of
        Ok payload ->
          (ShowCounties payload, Cmd.none)

        Err payload ->
          (Failure payload, Cmd.none)
    SaveZip zip5 ->
      (Zip5 zip5, Cmd.none)
    SubmitZip zip5->
      (Loading, getCounties zip5)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Title"
  , body = [
      div []
        [ h2 [] [ text "User Data" ]
      ]
      , div [] [ viewUser model ]
      , div []
          [ input [ placeholder "Zip5", value SaveZip, onInput confirmLength ] []
          , button  [ onClick SubmitZip, style "display" "block" ], [ text "Submit!" ]
          ]
      , div [] [ viewCounties model ]
    ]
  }


viewUser : Model -> Html Msg
viewUser model =
  case model of
    Failure payload ->
      div []
        [ p [] [ text "I could not load user data for some reason. " ]
        , p [] [ button [ onClick MorePlease ] [ text "Try Again!" ] ]
        , p [] [ text (errorToString payload) ]
        ]

    Loading ->
      text "Loading..."

    ShowUser payload ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , p [] [ text payload.username ]
        , p [] [ text payload.password ]
        ]
    ShowCounties _ ->
      p [] [ text "county"]
    SaveZip _ ->
      p [] [ text "zip5"]
    SubmitZip ->
      p [] [ text "submit" ]


viewCounties : Model -> Html Msg
viewCounties model =
  case model of
    Failure payload ->
      div []
        [ p [] [ text "I could not load zip data for some reason. " ]
        , p  [] [ text ( errorToString payload ) ]
        ]
    ShowCounties payload ->
      div []
        [ p [] [ text "Zip is:" ]
        , div [] [ renderList payload ]
        ]
    Loading ->
      p [] [ text "No zip entered."]
    ShowUser _ ->
      p [] [ text "Show User."]
    SaveZip _ ->
      p [] [ text "Zip5."]
    SubmitZip ->
      p [] [ text "Submit!"]

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

getUserData : Cmd Msg
getUserData =
  Http.get
    { url = "http://localhost:5000/api/user"
    , expect = Http.expectJson GotUser userDecoder
    }

usernameDecoder : Decoder String
usernameDecoder =
  field "username" string

passwordDecoder : Decoder String
passwordDecoder =
  field "password" string

userDecoder : Decoder User
userDecoder =
  map2 User usernameDecoder passwordDecoder

errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus _ ->
            "Unknown error"
        BadBody errorMessage ->
            errorMessage

renderList : List String -> Html msg
renderList lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []
