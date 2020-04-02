module Main exposing (..)

import Browser
import Html exposing (..)
import Task exposing (Task)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, string, at, map2, map3, maybe, null, oneOf)
import Json.Decode.Pipeline exposing (required, optional)
import Date exposing (Date, day, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Time exposing (Weekday(..), Month(..))
import Element

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model =
  { name : String
  , age :  ValidInt
  , zip : ValidInt
  , counties : List String
  , county : Maybe String
  , gender : String
  , tobacco : Bool
  , discounts : Bool
  , planN : Bool
  , planF: Bool
  , planG: Bool
  , preset : String
  , state : ViewState
  , date  : Maybe Date
  , datePicker : DatePicker.DatePicker
  , valid : Bool
  , response : Maybe PlanData
  , pdpList : Maybe (List PdpRecord)
  , pdpRate : Maybe String
  , recentError : String
  , today : Maybe Date
  }

type alias ValidInt =
  { value : Maybe Int
  , valid : Bool
  , comment : String
  }

type alias PdpRecord =
  { plan : String
  , rate : String
  }

type alias PlanQuote =
  { company : String
  , rate : String
  }

settings : DatePicker.Settings
settings = { defaultSettings | placeholder = Date.toIsoString <| Date.fromCalendarDate 2020 Jan 1 }

type ViewState
  = Failure Fail
  | Loading String
  | Ready
  | Valid
  | Success PlanData


init : () -> (Model, Cmd Msg)
init _ =
  let
    ( datePicker, datePickerFx ) =
      DatePicker.init
  in
    ( { name = ""
      , age = ValidInt Nothing False "Please enter an age"
      , zip = ValidInt Nothing False "Please enter a 5-digit ZIP"
      , counties = [""]
      , county = Nothing
      , gender = "M"
      , tobacco = False
      , discounts = False
      , planN = False
      , planF = False
      , planG = False
      , preset = "kansas_city"
      , state = Ready
      , date = Nothing
      , datePicker= datePicker
      , valid = False
      , response = Nothing
      , pdpList = Nothing
      , pdpRate = Nothing
      , recentError = ""
      , today = Nothing
      }
    , Cmd.map ToDatePicker datePickerFx
    )



-- UPDATE


type Msg
  = NoOp
  | SubmitForm
  | RequestPDP
  | SetName String
  | SetAge String
  | SetZip String
  | SelectGender String
  | SelectCounty String
  | SelectPreset String
  | SelectPDP String
  | ToggleTobacco
  | ToggleDiscounts
  | ToggleN
  | ToggleF
  | ToggleG
  | ZipResponse (Result Http.Error (List String))
  | PlanResponse (Result Http.Error PlanData)
  | PDPResponse (Result Http.Error (List PdpRecord))
  | ToDatePicker DatePicker.Msg
  | Reset -- is this going to return a string?
  | ReceiveDate Date

type Fail
  = Counties
  | PDP
  | Plan

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceiveDate td ->
      ( validateModel
      { model | today = Just td
              , date = Just td
        }
      , Cmd.none
      )

    NoOp ->
      ( model , Cmd.none )


    SubmitForm ->
      let
        vModel = validateModel model

      in
        if vModel.state == Valid then
          ( { vModel | response = Nothing
                     , state = Loading "Plan(s)"
            }
          , getPlans vModel
          )
        else
          ( { vModel | state = Ready }
          , Cmd.none
          )

    RequestPDP ->
      ( { model | state = Loading "PDP" }, getPDP model )

    SetName str ->
      case model.date of
        Nothing ->
          ( validateModel { model | name = str }
          ,  Date.today |> Task.perform ReceiveDate
          ) -- Would prefer to do this in init
        Just _ ->
          ( validateModel { model | name = str }
          ,  Cmd.none
          )

    SetAge str ->
      case String.toInt(str) of
        Just i ->
          let
            minAge = 65
            maxAge = 120
            errorMessage =
              if i <= minAge then
                "Age must be 65 or older"
              else if i > maxAge then
                "Seems too old; check age"
              else
                ""
            ageTest = (i >= minAge) && (i <= maxAge)
          in
            ( validateModel { model | age = ValidInt (Just i) ageTest errorMessage }
            , Cmd.none )

        Nothing ->
          ( validateModel { model | age = ValidInt Nothing False  "Must enter a valid integer" }
          , Cmd.none )

    SetZip str ->
      case String.toInt(str) of
        Just i ->
          if String.length(str) == 5 then
            let
              newModel = validateModel
                          { model | zip = ValidInt (Just i) True ""
                                  , state = Ready
                          }
            in
              ( newModel
              , getZip newModel
              )
          else
            ( validateModel { model | zip = ValidInt (Just i) False "Zip must be 5 digits long"
                            }
            , Cmd.none
            )
        Nothing ->
          ( validateModel { model | zip = ValidInt Nothing False "Zip must be a number" }
          , Cmd.none
          )

    SelectGender str ->
      let
        g = String.slice 0 1 str
      in
        ( validateModel { model | gender = g }
        , Cmd.none )

    SelectCounty str ->
      ( validateModel { model | county = Just str }
      , Cmd.none
      )

    SelectPreset str ->
      ( { model | preset = str }
      , Cmd.none )

    SelectPDP pr ->
      ( { model | pdpRate = Just pr }
      , Cmd.none )

    ToggleTobacco  ->
      ( { model | tobacco = not model.tobacco }
      , Cmd.none )

    ToggleDiscounts ->
      ( { model | discounts = not model.discounts }
      , Cmd.none )

    ToggleN ->
      ( validateModel { model | planN = not model.planN }
      , Cmd.none )

    ToggleF ->
      ( validateModel { model | planF = not model.planF }
      , Cmd.none )

    ToggleG ->
      ( validateModel { model | planG = not model.planG }
      , Cmd.none )

    ZipResponse rmsg ->
      case rmsg of
        Ok response ->
          (  validateModel
              { model | state = Ready
                      , counties = response
                      , county = List.head response
              }
          , getPDP model
          )
        Err error ->
          ( { model | state = Failure Counties}, Cmd.none )

    PlanResponse rmsg ->
      case rmsg of
        Ok response  ->
          ( { model | state = Success response
                    , response = Just response
          }
          , Cmd.none
          )
        Err error ->
          ( { model | state = Failure Plan
                    , recentError = errorToString error
            }
          , Cmd.none )

    PDPResponse rmsg ->
      case rmsg of
        Ok response ->
          (   { model | pdpList = Just response
                      , pdpRate = Just <| getRate <| List.head response
              }
          , Cmd.none
          )
        Err error ->
          ( { model | state = Failure PDP
                    , recentError = errorToString error
            }
          , Cmd.none )

    ToDatePicker subMsg ->
      let
        ( newDatePicker, dateEvent ) =
          DatePicker.update settings subMsg model.datePicker

        newDate =
            case dateEvent of
                Picked changedDate ->
                    Just changedDate
                _ ->
                    model.date
      in
        ( validateModel { model
            | date = newDate
            , datePicker = newDatePicker
          }
        , Cmd.none )

    Reset ->
      init ()



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container"]
    [ img [ src "images/logo.png"
          , height 100
          , width 360
          ]
          [  ]
    , variousViews model
    ]

variousViews : Model -> Html Msg
variousViews model =
  case model.state of
    Failure fail ->
      case fail of
        Counties ->
          div []
            [ text "I could not load ZIPs for some reason. "
            , button [ onClick Reset , style "display" "block" ] [ text "Reset" ]
            , div [] [ text model.recentError]
            ]
        PDP ->
          div []
            [ text "I could not load PDP for some reason. "
            , button [ onClick Reset , style "display" "block" ] [ text "Reset" ]
            , button [ onClick RequestPDP, style "display" "block" ] [ text "Resubmit PDP"]
            , div [] [ text model.recentError]
            ]
        Plan ->
          div []
            [ text "I could not load Plan(s) for some reason. "
            , button [ onClick Reset , style "display" "block" ] [ text "Reset" ]
            , button [ onClick SubmitForm, style "display" "block" ] [ text "Resubmit Plan Form"]
            , div [] [ text model.recentError]
            ]

    Ready ->
      div [ ]
        [ div []
          [ renderForm model SubmitForm "Submit" ]
        , renderResults model
        ]

    Valid ->
      div [ ]
        [ div []
          [ renderForm model SubmitForm "Submit" ]
        , renderResults model
        ]

    Loading str ->
      div [ ]
          [ div []
            [ renderForm model SubmitForm "Submit" ]
          , text <| "Loading " ++ str ++ "...."
          ]

    Success pd ->
      div [ ]
        [ div []
          [ renderForm model SubmitForm "Submit" ]
        , renderResults model
          ]




-- Model Validations

isValid : Model -> Bool
isValid model =
  case model.date of
    Just _ ->
      let
        validList =
          [ String.length(model.name) > 0
          , model.age.valid == True
          , model.zip.valid == True
          , model.planN || model.planF || model.planG
          ]
        newModel = { model | valid = List.foldl (&&) True validList }
      in
        newModel.valid
    Nothing ->
      False

validateModel : Model -> Model
validateModel model =
  case (isValid model) of
    True ->
      { model | valid = True
              , state = Valid
      }
    False ->
      { model | valid = False
      }



-- View Validations
validateVI : ValidInt -> Html Msg
validateVI field =
  case field.valid of
    True ->
      div [ style "color" "green" ] [ text ""]
    False ->
      div [ style "color" "red" ] [ text field.comment ]

validateString : String -> (String -> Bool) -> String -> Html Msg
validateString field func errorMessage=
  if func field then
    div [ style "color" "green" ] [ text ""]
  else
    div [ style "color" "red" ] [ text errorMessage ]

validateDate : Maybe Date -> Html Msg
validateDate field =
  case field of
    Just _ ->
      text ""
    Nothing ->
      p [ style "color" "red" ] [ text "No date" ]


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


-- HTML Rendering


renderForm : Model -> Msg -> String -> Html Msg
renderForm model func buttonLabel =
  Html.form
    [ onSubmit func
    ]
    ( List.map
        (\a -> (div [ class "row" ] [ a ] ) )
        [ textbox  "Name" "John Smith" model.name SetName "four columns"
        , textboxCheck  "Age" "65" model.age SetAge (validateVI model.age) "two columns"
        , textboxCheck  "ZIP" "12345" model.zip SetZip (validateVI model.zip) "two columns"
        , selectbox "County" model.counties SelectCounty "three columns"
        , selectbox  "Gender" ["Male", "Female"] SelectGender "three columns"
        , selectdate model "six columns"
        , checkbox  "Tobacco User?" model.tobacco ToggleTobacco  "u-full-width"
        , checkbox  "Apply Discounts?" model.discounts ToggleDiscounts "u-full-width"
        , h5 [ class "u-full-width" ] [ text "Which Plans?"]
        , checkbox "Plan N" model.planN ToggleN "u-full-width"
        , checkbox "Plan F" model.planF ToggleF "u-full-width"
        , checkbox "Plan G" model.planG ToggleG "u-full-width"
        , selectbox
              "Preset"
              [ "kansas_city", "st_louis_il", "st_louis_mo", "top_ten", "all" ]
              SelectPreset
              "three columns"
        , button [ style "block" "display", class "button-primary", disabled (not model.valid) ] [ text "Submit" ]
        ]
    )

renderResults : Model -> Html Msg
renderResults model =
  case model.response of
    Just pd ->
      div []
          ( List.map
            (\a -> ( div [ class "row" ] [a] ) )
            [ button [ onClick Reset, style "display" "block" ] [ text "Reset" ]
            , button [ onClick SubmitForm, style "display" "block" ] [ text "Resubmit" ]
            , pdpSelectBox model.pdpList (\a -> SelectPDP a)
            , p [ class "six columns"] [ text " We seem to have data :" ]
            , renderPlans pd.planF "Plan F"
            , renderPlans pd.planG "Plan G"
            , renderPlans pd.planN "Plan N"
            ]
          )
    Nothing ->
      div []
          [ text "" ]

renderPlans : Maybe (List PlanQuote) -> String -> Html Msg
renderPlans ppd lab =
  case ppd of
    Just pd ->
      div []
        [ h3 [] [ text lab ]
        , table
          []
          ([ thead []
              [ th [] [ text "Company" ]
              , th [] [ text "Rate" ]
              ]
          ]
            ++ List.map toTableRow pd
          )
        ]
    Nothing ->
      text ""



toTableRow : PlanQuote -> Html Msg
toTableRow pq =
  tr []
    [ td [] [ text pq.company ]
    , td [] [ text pq.rate ]
    ]

renderList : List String -> Html Msg
renderList lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []

selectdate : Model -> String -> Html Msg
selectdate model class_ =
  div [ class class_ ] [
    label
      [ ]
      [ text "Pick a date:"
      , DatePicker.view model.date settings model.datePicker
        |> Html.map ToDatePicker
      ]
  ]

selectbox : String -> List (String) -> (String -> Msg) -> String -> Html Msg
selectbox title_ choices handle class_ =
  div [ class class_ ] [
    label
      [ ]
      [ text title_
      , select
        [ onInput handle
        , class "u-full-width"
        ]
        (List.map
          (\a -> option [ value a ] [ text a ])
          choices
        )
      ]
  ]

pdpOption : PdpRecord -> Html Msg
pdpOption pr =
  option [ value pr.rate ] [ text pr.plan ]

pdpSelectBox : Maybe (List PdpRecord) -> (String -> Msg) -> Html Msg
pdpSelectBox mplist handle =
  case mplist of
    Just plist ->
      div [class "six columns"] [
        label
          [ ]
          [ span [ class "label-body"] [ text "Select PDP:"]
          , select
            [ onInput handle , class "u-full-width"]
            ( List.map
                pdpOption
                plist
            )
          ]
      ]
    Nothing ->
      p [] [
        label
          []
          [ select
            [ onInput handle ]
            [ text "" ]
          ]
      ]


checkbox : String -> Bool -> Msg -> String -> Html Msg
checkbox title_ fvalue handle class_=
    div [ class class_ ] [
      label
        [ ]
        [ input [ type_ "checkbox", checked fvalue, onClick handle ] []
        , span [ class "label-body" ] [ text title_ ]
        ]
    ]

textbox : String -> String -> String -> (String -> Msg) -> String -> Html Msg
textbox title_ placeholder_ fvalue handle class_ =
  div [ class class_ ] [
    label
      [ ]
      [ text title_
      , input [ type_ "text", class "u-full-width", placeholder placeholder_, value fvalue, onInput handle ] []
      ]
  ]

textboxCheck : String -> String -> ValidInt -> (String -> Msg) -> Html Msg -> String -> Html Msg
textboxCheck title_ placeholder_ fvalue handle validator class_ =
  case fvalue.value of
    Just i ->
      div [ class class_ ] [
        label
          [  ]
          [ text title_
          , input [ type_ "text", class "u-full-width", placeholder placeholder_, value (String.fromInt i), onInput handle ] []
          , validator
          ]
      ]
    Nothing ->
        div [ class class_ ] [
          label
            [  ]
            [ text title_
            , input [ type_ "text", class "u-full-width", placeholder placeholder_, value "", onInput handle ] []
            ]
        ]



-- MISC type conversions

boolString : Bool -> String
boolString b =
  if b then
    "True"
  else
    "False"

stringMaybeInt : Maybe Int -> String
stringMaybeInt v =
  case v of
    Just i ->
      String.fromInt(i)
    Nothing ->
      ""


formatDate : Maybe Date -> String
formatDate dd =
  case dd of
    Just d ->
      Date.toIsoString d
    Nothing ->
      ""
strCounty : Maybe String -> String
strCounty c =
  case c of
    Just s ->
      s
    Nothing ->
      ""

getRate : Maybe PdpRecord -> String
getRate pr =
  case pr of
    Just p ->
      p.rate
    Nothing ->
      ""

-- HTTP Requests & JSON

getZip : Model -> Cmd Msg
getZip model =
  let
    zip = model.zip.value
  in
    case zip of
      Just z ->
        Http.get
          { url = "https://enlightnu-quote-api.herokuapp.com/api/counties?zip=" ++ String.fromInt(z)
          , expect = Http.expectJson ZipResponse countyDecoder
          }
      Nothing ->
        Http.get
          { url = "https://enlightnu-quote-api.herokuapp.com/api/counties?zip=" ++ ""
          , expect = Http.expectJson ZipResponse countyDecoder
          }

countyDecoder : Decoder (List (String))
countyDecoder =
  field "zip" (Json.Decode.list string)

checkAddPlan : Bool -> String -> String -> String
checkAddPlan b plan str =
  if b then
    str ++ "&plan=" ++ plan
  else
    str

getPlans : Model -> Cmd Msg
getPlans model =
  if model.state == Valid then
    let
      url1 =  "https://enlightnu-quote-api.herokuapp.com/api/plans?"
      url2 = url1
          ++ "zip=" ++ ( stringMaybeInt model.zip.value )
          ++ "&age=" ++ ( stringMaybeInt model.age.value )
          ++ "&county=" ++ ( strCounty model.county )
          ++ "&gender=" ++ model.gender
          ++ "&tobacco=" ++ ( model.tobacco |> boolString )
          ++ "&discounts=" ++ ( model.discounts |> boolString )
          ++ "&date=" ++ ( formatDate model.date )
          ++ "&preset=" ++ model.preset

      url3 =  checkAddPlan model.planN "N" url2
      url4 =  checkAddPlan model.planF "F" url3
      url5 =  checkAddPlan model.planG "G" url4

    in
      Http.get
        { url = url5
        , expect = Http.expectJson PlanResponse planXDecoder
        }
  else
    Cmd.none

planRateDecoder : Decoder PlanQuote
planRateDecoder =
  map2
    PlanQuote
    ( field "company" string )
    ( field "rate" string )

type alias PlanData =
  { planF : Maybe (List PlanQuote)
  , planG : Maybe (List PlanQuote)
  , planN : Maybe (List PlanQuote)
  }

planListDecoder : Decoder (List PlanQuote)
planListDecoder =
  Json.Decode.list planRateDecoder


planXDecoder : Decoder PlanData
planXDecoder =
  map3 PlanData
    ( maybe ( field "F" planListDecoder ) )
    ( maybe ( field "G" planListDecoder ) )
    ( maybe ( field "N" planListDecoder ) )


getPDP : Model -> Cmd Msg
getPDP model =
  Http.get
    { url = "https://enlightnu-quote-api.herokuapp.com/api/pdp?zip=" ++ ( stringMaybeInt model.zip.value )
    , expect = Http.expectJson PDPResponse pdpDecoder
    }

pdpPlanDecoder : Decoder PdpRecord
pdpPlanDecoder =
  map2
    PdpRecord
    ( field "Plan Name" string )
    ( field "rate" string )

pdpDecoder : Decoder (List PdpRecord)
pdpDecoder =
  field "body" ( Json.Decode.list pdpPlanDecoder )
