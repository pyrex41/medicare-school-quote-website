module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN

main =
  Browser.sandbox { init = init, update = update,  view = view}


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

type Validation
  = None
  | Ok
  | Error String

init : Model
init =
  Model "" "" ""


--  UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model  | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewInput "text" "Name" model.name Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    ]

validate : Model  -> Validation
validate model =
  if String.length model.name == 0 then
    Error "Please enter a name"
  else if String.length model.password <= 8 then
    Error "Password must be at least 8 characters long"
  else if not (String.any Char.isDigit model.password) then
    Error "Password must have at least 1 number"
  else if not (String.any Char.isUpper model.password) then
    Error "Password must have at least one uppercase letter"
  else if not (String.any Char.isLower model.password) then
    Error "Password must have at least one lowercase letter"
  else if model.password /= model.passwordAgain then
    Error "Passwords do not match"
  else
    Ok


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model =
  case validate model of
    Ok ->
      div [ style "color" "green" ] [ text "OK" ]
    Error err ->
      div [ style "color" "red" ] [ text err ]
    None ->
      div [ style "color" "black" ] [ text "Enter your details"]
