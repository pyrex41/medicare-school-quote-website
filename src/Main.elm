module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder
import Url.Parser exposing (Parser, (</>), oneOf)
import Html exposing (..)
import Task exposing (Task)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (Decoder, field, string, at, map2, map3, map4,map5, maybe, null, oneOf, int)
import Element exposing (Element, rgb, rgba, Column)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Table exposing (defaultCustomizations)
import Dict exposing (Dict)
import Time exposing (Month, utc)
import MyDate exposing (CustomDate, addMonth)
import Round
import Presets exposing (naicCategory, displayNames)


-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , name : String
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
  , state : ViewState
  , date  : Maybe CustomDate
  , valid : Bool
  , response : Maybe (List PlanQuote)
  , pdpList : Maybe (List PdpRecord)
  , pdpSelect : Maybe String
  , partB : Maybe String
  , recentError : String
  , today : Maybe CustomDate
  , tableState : Table.State
  , tableRows : Maybe (List TableRow)
  , viewPreferred : Bool
  , viewNonpreferred : Bool
  , viewOutside : Bool
  , timeNow : Maybe CustomDate
  , dateSelectChoices : List (String, CustomDate)
  , outputTableState : Table.State
  , outputQuotes : Maybe (List OutputQuote)
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

type RowCategory
  = Preferred
  | NonPreferred
  | Outside

type alias PlanQuote =
  { company : String
  , fRate : Maybe String
  , gRate : Maybe String
  , nRate : Maybe String
  , naic : Int
  }

type alias TableRow =
  { company : String
  , displayName : String
  , fRate : String
  , gRate : String
  , nRate : String
  , naic : Int
  , selected : Bool
  , category : RowCategory
  , priority : Int
  }

type alias OutputQuote =
  { planName : String
  , displayName : String
  , quote : String
  , drugPremium : String
  , partBPremium : String
  }

type ViewState
  = Failure Fail
  | Loading
  | Ready
  | Results
  | Output

type PlanType
  = G
  | N
  | F




init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
    ( { key = key
      , url = url
      , name = ""
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
      , state = Ready
      , date = Nothing
      , valid = False
      , response = Nothing
      , pdpList = Nothing
      , pdpSelect = Nothing
      , partB = Just "$144.60"
      , recentError = ""
      , today = Nothing
      , tableState = Table.initialSort "Category"
      , tableRows = Nothing
      , viewPreferred = True
      , viewNonpreferred = False
      , viewOutside = False
      , timeNow = Nothing
      , dateSelectChoices = []
      , outputTableState = Table.initialSort "Title"
      , outputQuotes = Nothing
      }
    , Task.perform GotTime Time.now
    )


-- URL ROUTING

routeParser : Parser (ViewState -> a) a
routeParser =
  Url.Parser.oneOf
    [ Url.Parser.map Ready    (Url.Parser.s "home")
    , Url.Parser.map Results  (Url.Parser.s "results")
    , Url.Parser.map Output   (Url.Parser.s "output")
    ]

urlToRoute : Url.Url -> ViewState
urlToRoute url =
  url
    |> Url.Parser.parse routeParser
    |> Maybe.withDefault Ready


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
  | SelectDate String
  | SelectPDP String
  | ToggleTobacco
  | ToggleDiscounts
  | ToggleN
  | ToggleF
  | ToggleG
  | ZipResponse (Result Http.Error (List String))
  | PlanResponse (Result Http.Error (List PlanQuote))
  | PDPResponse (Result Http.Error (List PdpRecord))
  | ReceiveDate CustomDate
  | SetTableState Table.State
  | TogglePreferred
  | ToggleNonPreferred
  | ToggleOutside
  | DeselectAll
  | SelectAll (Maybe (List Int))
  | GotTime Time.Posix
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | ShowOutput
  | ToggleSelect Int

type Fail
  = Counties
  | PDP
  | Plan


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowOutput ->
      let
        curl = model.url
        nurl = { curl | path = "/output" }
      in
        ( { model | url = nurl
                  , state = Output }
        , Nav.pushUrl model.key (Url.toString nurl))

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href)

    UrlChanged url ->
      ( { model | state = urlToRoute url }
      , Cmd.none
      )

    SetTableState newState ->
      ( { model | tableState = newState }
      , Cmd.none
      )

    ReceiveDate td ->
      ( validateModel
        { model | today = Just td }
      , Cmd.none
      )

    NoOp ->
      ( model , Cmd.none )


    SubmitForm ->
      let
        vModel = validateModel model

      in
        if vModel.valid then
          ( { vModel | response = Nothing
                     , state = Loading
            }
          , getPlans vModel
          )
        else
          ( { vModel | state = Ready }
          , Cmd.none
          )

    RequestPDP ->
      ( { model | state = Loading }, getPDP model )

    SetName str ->
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

    SelectDate cds ->
      let
        choices_ = model.dateSelectChoices
        choiceTuple = List.head <| List.filter
                                    (\a -> Tuple.first a == cds)
                                    choices_
        choice = Maybe.map Tuple.second choiceTuple
      in
        ( validateModel { model | date = choice }
        , Cmd.none
        )

    SelectPDP pr ->
      ( { model | pdpSelect = Just pr }
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

    TogglePreferred ->
      let
        newBool = not model.viewPreferred
        newRows = case model.tableRows of
          Just tr ->
            Just <| setRows Preferred newBool tr
          Nothing ->
            Nothing
      in
        ( { model | viewPreferred = newBool
                  , tableRows = newRows
                  , tableState = Table.initialSort "category"
          }
        , Cmd.none )

    ToggleNonPreferred ->
      let
        newBool = not model.viewNonpreferred
        newRows = case model.tableRows of
          Just tr ->
            Just <| setRows NonPreferred False tr
          Nothing ->
            Nothing
      in
        ( { model | viewNonpreferred = newBool
                  , tableRows = newRows
                  , tableState = Table.initialSort "category"
          }
        , Cmd.none )

    ToggleOutside ->
      let
        newBool = not model.viewOutside
        newRows = case model.tableRows of
          Just tr ->
            Just <| setRows Outside False tr
          Nothing ->
            Nothing
      in
        ( { model | viewOutside = newBool
                  , tableRows = newRows
                  , tableState = Table.initialSort "category"
          }
        , Cmd.none )

    ToggleSelect i ->
      ( { model | tableRows =
                        Maybe.map
                          ( List.map (toggle i) )
                          model.tableRows
        }
      , Cmd.none)

    DeselectAll ->
      let
        newRows = case model.tableRows of
          Just tr ->
            Just <| List.map (\a -> { a | selected = False } ) tr
          Nothing ->
            Nothing
      in
        ( { model | tableRows = newRows }
        , Cmd.none )

    SelectAll ils ->
      let
        newRows = case model.tableRows of
          Just tr ->
            case ils of
              Just ii ->
                Just <| List.map ( selectByNaic ii ) tr
              Nothing ->
                Nothing
          Nothing ->
            Nothing
      in
        ( { model | tableRows = newRows }
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
          let
            newRows = List.map planToRow response
            curl = model.url
            nurl = { curl | path = "/results" }
          in
            ( { model | response = Just response
                      , tableRows = Just newRows
                      , url = nurl
                      , state = Results
              }
            , Nav.pushUrl model.key (Url.toString nurl)
            )
        Err error ->
          let
            curl = model.url
            eurl = { curl | path = "/error" }
          in
            ( { model | state = Failure Plan
                      , recentError = errorToString error
                      , url = eurl
              }
            , Nav.pushUrl model.key (Url.toString eurl)
            )

    PDPResponse rmsg ->
      case rmsg of
        Ok response ->
          let
            pr_sort = List.sortBy .plan response
            prs = case (List.head pr_sort) of
              Just pr ->
                Just pr.rate
              Nothing ->
                Nothing

          in
            (   { model | pdpList = Just pr_sort
                        , pdpSelect = prs
                }
            , Cmd.none
            )
        Err error ->
          ( { model | state = Failure PDP
                    , recentError = errorToString error
            }
          , Cmd.none )

    GotTime timenow ->
      let
        td = CustomDate
                    (Time.toMonth utc timenow)
                    (Time.toYear utc timenow)

        choices_ = List.map
                    (\a -> Tuple.pair (MyDate.toString (addMonth a td) ) (addMonth a td))
                    [0,1,2,3]

        choiceVals = List.map Tuple.second choices_
        firstChoice = Maybe.andThen List.head (List.tail choiceVals)
      in
        ( { model | timeNow = Just td
                  , date = firstChoice
                  , dateSelectChoices = choices_
          }
        , Cmd.none
        )


-- UPDATE FUNCS
toggle : Int -> TableRow -> TableRow
toggle i tablerow =
  if tablerow.naic == i then
    { tablerow | selected = not tablerow.selected }
  else
    tablerow

selectByNaic : (List Int) -> TableRow -> TableRow
selectByNaic  ls tablerow =
  if (List.member tablerow.naic ls) then
    { tablerow | selected = True }
  else
    tablerow

removeRow : Int -> List TableRow -> List TableRow
removeRow i ls =
  List.filter
    (\a -> a.naic /= i)
    ls

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Enlightnu Quoting App"
  , body =
      [ div [ class "container"
            , style "max-width" "100%"
            , style "heigt" "auto"
            ]
        [ img [ src "images/logo.png"
              , style "max-width" "100%"
              , style "height" "auto"
              ]
              [  ]
        , variousViews model
        ]
      ]
  }


-- v---v FOR TESTING

viewCustomDate : Model -> Html Msg
viewCustomDate model =
  case model.timeNow of
    Just tn ->
      let
        options = List.map Tuple.first model.dateSelectChoices
        md = case model.date of
          Just d ->
            MyDate.toString d
          Nothing ->
            "none"
      in
        div []
          (List.map
            (\a -> text (a ++ " === ") )
            options
          ++
          [ text <| "Default: " ++ md ]
          )
    Nothing ->
      div []
        [ text "no time info" ]

variousViews : Model -> Html Msg
variousViews model =
  case model.state of
    Failure fail ->
      case fail of
        Counties ->
          div []
            [ text "I could not load ZIPs for some reason. "
            , div [] [ text model.recentError]
            ]
        PDP ->
          div []
            [ text "I could not load PDP for some reason. "
            , button [ onClick RequestPDP, style "display" "block" ] [ text "Resubmit PDP"]
            , div [] [ text model.recentError]
            ]
        Plan ->
          div []
            [ text "I could not load Plan(s) for some reason. "
            , button [ onClick SubmitForm, style "display" "block" ] [ text "Resubmit Plan Form"]
            , div [] [ text model.recentError]
            ]

    Ready ->
      div [ ]
        [ div []
          [ renderForm model SubmitForm "Submit" ]
        ]

    Loading ->
      div [ ]
          [ div [ class "row" ]
            [ renderForm model SubmitForm "Submit" ]
          ]

    Results ->
      div [ ]
        [ div []
          [ div [] [ renderResults model] ]
        ]

    Output ->
      div [ ]
          [ div []
            [ renderOutput model ]--renderOutput model ]
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
          , model.county /= Nothing
          ]
        newModel = { model | valid = List.foldl (&&) True validList }
      in
        newModel.valid
    Nothing ->
      False

validateModel : Model -> Model
validateModel model =
  { model | valid = isValid model }



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


viewCheckbox : TableRow -> Table.HtmlDetails Msg
viewCheckbox { selected } =
    Table.HtmlDetails []
        [ input [ type_ "checkbox", checked selected ] []
        ]


renderForm : Model -> Msg -> String -> Html Msg
renderForm model func buttonLabel =
  let
    loadText = if model.state == Loading then "Loading..." else ""
  in
    Html.form
      [ onSubmit func
      ]
      ( List.map
          (\a -> (div [ class "row" ] [ a ] ) )
          [ textbox  "Name" "John Smith" model.name SetName "four columns"
          , textboxCheck  "Age" "65" model.age SetAge (validateVI model.age) "two columns"
          , textboxCheck  "ZIP" "12345" model.zip SetZip (validateVI model.zip) "two columns"
          , selectbox "County" model.counties SelectCounty "three columns" 0
          , selectbox  "Gender" ["Male", "Female"] SelectGender "three columns" 0
          , selectbox "Effective Date" (List.map Tuple.first model.dateSelectChoices) SelectDate "three columns" 1
          , checkbox  "Tobacco User?" model.tobacco ToggleTobacco  "u-full-width"
          , checkbox  "Apply Household Discount?" model.discounts ToggleDiscounts "u-full-width"
          , h5 [ class "u-full-width" ] [ text "Which Plans?" ]
          , checkbox "Plan G" model.planG ToggleG "u-full-width"
          , checkbox "Plan N" model.planN ToggleN "u-full-width"
          , checkbox "Plan F" model.planF ToggleF "u-full-width"
          , div [ class "row" ]
              [ div [ class "two columns" ]
                  [ button [ style "block" "display", class "button-primary", disabled (not model.valid) ] [ text "Submit" ] ]
              , div [ class "one column" ] [ text <| loadText ]
              ]
          ]
      )

renderResults : Model -> Html Msg
renderResults model =
  let
    showRows = viewRowsAll model
    naicShow = case showRows of
      Just sr ->
        Just <| List.map (\a -> a.naic) sr
      Nothing ->
        Nothing
  in
    div []
      [ div [ class "row" ] [ pdpSelectBox model.pdpList model.pdpSelect (\a -> SelectPDP a) ]
      , div [ class "row" ]
          [ checkbox "Preferred Plans" model.viewPreferred TogglePreferred "u-full-width"
          , checkbox "Non-Preferred Plans" model.viewNonpreferred ToggleNonPreferred "u-full-width"
          , checkbox "Outside Plans" model.viewOutside ToggleOutside "u-full-width"
          ]
      , div [ class "row" ]
          [ div [ class "two columns" ]
              [ div [ class "u-full-width" ]
                    [ button
                      [ onClick ShowOutput, class "button-primary" ]
                      [ text "Show Output" ]
                    ]
              ]
          , div [ class "two columns"]
              [ div [ class "u-full-width" ]
                    [ button
                      [ onClick ( SelectAll naicShow ), class "button" ]
                      [ text "Select All" ]
                    ]
              ]
          , div [ class "two columns"]
              [ div [ class "u-full-width" ]
                    [ button
                      [ onClick DeselectAll, class "button" ]
                      [ text "Deselect All" ]
                    ]
              ]
          ]
      , div [ class "row" ]
          [ case showRows of
              Just sr ->
                Table.view config model.tableState sr
              Nothing ->
                Table.view config model.tableState []
          ]
            --div [] [ text "" ]
      , button [ onClick SubmitForm, style "display" "block" ] [ text "Resubmit" ]
      ]

renderOutput : Model -> Html msg
renderOutput model =
  let
    tl = List.map2 Tuple.pair [model.planG, model.planN, model.planF] [G,N,F]
    tlf = List.filter (\a -> Tuple.first(a)) tl
    pl = List.map Tuple.second tlf
    tables = List.map (outputTable model) pl
  in
    div [] <| [(personalInfo model)] ++ tables

-- Output Page Utils
personalInfo : Model -> Html msg
personalInfo model =
  let
    pdpText = case model.pdpSelect of
      Just s -> s
      Nothing -> ""
    ageText = case model.age.value of
      Just a -> String.fromInt(a)
      Nothing -> ""
    zipText = case model.zip.value of
      Just v -> String.fromInt(v)
      Nothing -> ""
    row2 = ageText ++ " yrs" ++ "   |   " ++ zipText ++ "   |   " ++ model.gender
  in
    div []
      [ div [ class "row" ]
        [ strong [] [ text model.name ] ]
      , div [ class "row"]
        [ text row2 ]
      , div [ class "row" ]
        [ text pdpText ]
      ]


outputTable : Model -> PlanType -> Html msg
outputTable model pt =
  case model.tableRows of
    Just tr ->
      let
        pText = case pt of
          G -> "PLAN G"
          N -> "PLAN N"
          F -> "PLAN F"
        vr = List.sortBy ( \a -> a.displayName ) <| List.filter (\a -> a.selected) tr
        companyNames = toHeadRow pText <| List.map .displayName vr
        rates = rateUtil pt vr
        rateRow = toBodyRow (pTextUtil pt) [] rates
        pdp = safeCurrencyFloat model.pdpSelect
        pdpRow = toBodyRow "Drug Plan Monthly Premium" [] <| List.map (\a -> safeString model.pdpSelect) vr
        insuranceTotal = List.map (\r -> currencyAddTwo pdp (safeCurrencyFloat (Just r))) rates
        insuranceTotalRow = simpleTotalRow "Insurance Monthly Total" insuranceTotal
        partb = safeCurrencyFloat model.partB
        partBRow = toBodyRow "Part B Rate" [] <| List.map (\a -> safeString model.partB) vr
        grandTotal = List.map (\t -> currencyAddThree pdp partb (safeCurrencyFloat (Just t))) rates
        grandTotalRow = simpleTotalRow "Grand Monthly Total" grandTotal
      in
        div []
            [ table [ class "u-full-width", style "table-layout" "fixed" ]
                [ thead [] [ companyNames ]
                , tbody []
                  [ rateRow
                  , pdpRow
                  , insuranceTotalRow
                  , partBRow
                  , grandTotalRow
                  ]
                ]
            , hr [] []
            ]
    Nothing -> text "No Output Available"


pTextUtil : PlanType -> String
pTextUtil pt =
  case pt of
    G -> "Plan G Monthly Premium"
    N -> "Plan N Monthly Premium"
    F -> "Plan F Monthly Premium"

rateUtil : PlanType -> List TableRow -> List String
rateUtil pt ls =
  case pt of
    G -> List.map .gRate ls
    N -> List.map .nRate ls
    F -> List.map .fRate ls

-- TABLE CONFIGURATION

categoryColumn : Table.Column TableRow Msg
categoryColumn =
  Table.customColumn
    { name = "Category"
    , viewData = categoryLabel << .category --String.fromInt << .priority
    , sorter = Table.increasingOrDecreasingBy .priority
    }

setRows : RowCategory -> Bool -> List TableRow -> List TableRow
setRows cat b trls =
  List.map
    ( \a ->
        if a.category == cat then
          { a | selected  = b }
        else
          a
    )
    trls


categoryLabel : RowCategory -> String
categoryLabel r =
  case r of
    Preferred -> "Preferred"
    NonPreferred -> "Non-Preferred"
    Outside -> "Outside"

viewRows : Bool -> RowCategory -> Maybe (List TableRow) -> Maybe ( List TableRow)
viewRows b c l =
  if b then
    case l of
      Just ll ->
        Just <| List.sortBy .displayName <| List.filter (\a -> a.category == c) ll
      Nothing ->
        Nothing
  else
    Nothing

viewRowsAll : Model -> Maybe (List TableRow)
viewRowsAll model =
  let
    showPreferred = viewRows model.viewPreferred Preferred model.tableRows
    showNonPreferred = viewRows model.viewNonpreferred NonPreferred model.tableRows
    showOutside = viewRows model.viewOutside Outside model.tableRows
  in
    safeConcat [showPreferred, showNonPreferred, showOutside]

safeAppend : Maybe (List a) -> Maybe (List a) -> Maybe (List a)
safeAppend a b =
  case a of
    Just aa ->
      case b of
        Just bb ->
          Just <| List.append aa bb
        Nothing ->
          a
    Nothing ->
      case b of
        Just bb ->
          b
        Nothing ->
          Nothing

safeConcat : List (Maybe (List a)) -> Maybe (List a)
safeConcat l =
  List.foldr safeAppend Nothing l


config : Table.Config TableRow Msg
config =
  Table.customConfig
      { toId = .company
      , toMsg = SetTableState
      , columns =
          [ checkboxColumn
          , Table.stringColumn "Company" .displayName
          , Table.stringColumn "Full Name" .company
          , Table.stringColumn "G Rate" .gRate
          , Table.stringColumn "N Rate" .nRate
          , Table.stringColumn "F Rate" .fRate
          , categoryColumn
          ]
      , customizations =
          { defaultCustomizations | rowAttrs = toRowAttrs }
      }

toRowAttrs : TableRow -> List (Attribute Msg)
toRowAttrs tablerow =
    [ onClick (ToggleSelect tablerow.naic)
    , style "background"
        (if tablerow.selected then
            "#CEFAF8"

         else
            "white"
        )
    ]

checkboxColumn : Table.Column TableRow Msg
checkboxColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewCheckbox
        , sorter = Table.unsortable
        }

renderList : List String -> Html Msg
renderList lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []


selectbox : String -> List (String) -> (String -> Msg) -> String -> Int -> Html Msg
selectbox title_ choices handle class_ i =
  let
    def = List.drop i choices |> List.head
    nls = List.map
            (\a -> option
                    [ value a
                    , selected <| (Just a) == def
                    ]
                    [ text a ])
            choices
  in
    div [ class class_ ] [
      label
        [ ]
        [ text title_
        , select
          [ onInput handle
          , class "u-full-width"
          ]
          nls
        ]
    ]

defselectbox : String -> String -> List (String) -> (String -> Msg) -> String -> Int -> Html Msg
defselectbox title_ def choices handle class_ i =
  let
    nls = List.map
            (\a -> option
                    [ value a
                    , selected <| a == def
                    ]
                    [ text a ])
            choices
  in
    div [ class class_ ] [
      label
        [ ]
        [ text title_
        , select
          [ onInput handle
          , class "u-full-width"
          ]
          nls
        ]
    ]

pdpOption : Maybe String ->  PdpRecord -> Html Msg
pdpOption def pr =
  let
    p_name =
      if String.endsWith "(PDP)" (String.trimRight pr.plan) then
        String.slice 0 -6 pr.plan
      else
        pr.plan
    r_val = pr.rate
    p_name_pad = String.padRight 50 ' ' p_name
    p_text = p_name_pad ++ r_val
  in
    option [ value pr.rate,  selected <| (Just pr.rate) == def ] [ text p_text ]

pdpSelectBox : Maybe (List PdpRecord) -> Maybe String -> (String -> Msg) -> Html Msg
pdpSelectBox mplist selectedPdp handle =
  case mplist of
    Just plist ->
      div [class "six columns"] [
        label
          [ ]
          [ span [ class "label-body"] [ text "Prescription Dug Plan:"]
          , select
            [ onInput handle , class "u-full-width"]
            ( List.map
                (pdpOption selectedPdp)
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

-- Output Table View funcs



toHeadRow : String -> (List String) -> Html msg
toHeadRow rowname l =
  let
    ls = [rowname] ++ l
  in
    tr []
      <| List.map
          (\a ->
              th [] [ text a ]
          )
          ls

totalRow  : String -> String -> String -> (List String) -> Html msg
totalRow rowname col1 col2 l =
  let
    ls = [rowname] ++ l
    mm = List.minimum <| List.filter (\a -> a /= "$ ---.--") l
    m = case mm of
      Just n ->
        n
      Nothing ->
        ""
  in
    tr []
      <| List.map
          (\a ->
              td
                (customBackground col1 col2 m a)
                [ text a ]
          )
          ls

simpleTotalRow : String -> List String -> Html msg
simpleTotalRow rowname l =
  let
    ls = [rowname] ++ l
  in
    tr [] <| List.map
                ( \a -> td [ style "background" "#d3d3d3" ] [ text a ] )
                ls

customBackground : String -> String -> String -> String -> List (Attribute msg)
customBackground col1 col2 tv v =
  let
    c = if tv == v then
          col2
        else
          col1
  in
    [ style "background" c ]

toBodyRow : String -> List (Attribute msg)-> (List String) -> Html msg
toBodyRow rowname attrs l =
  let
    ls = [rowname] ++ l
  in
    tr []
      <| List.map
          (\a ->
              td attrs [ text a ]
          )
          ls


currencyAddTwo : Float -> Float -> String
currencyAddTwo a b =
  if b == 0 then
    "$ ---.--"
  else
    "$" ++ (Round.round 2 (a+b))

currencyAddThree : Float -> Float -> Float -> String
currencyAddThree a b c =
  if c == 0 then
    "$ ---.--"
  else
    "$" ++ (Round.round 2 (a+b+c))


-- MISC type conversions

safeShowPDP : Maybe PdpRecord -> String
safeShowPDP pr =
  case pr of
    Just p ->
      p.rate
    Nothing ->
      ""

safeString : Maybe String -> String
safeString ms =
  case ms of
    Just s ->
      s
    Nothing ->
      ""

planToRow : PlanQuote -> TableRow
planToRow pq =
  let
    category = findCategory pq.naic
    showRowInit = category == Preferred
    priority = case category of
      Preferred -> 1
      NonPreferred -> 2
      Outside -> 3
    displayName = Maybe.withDefault pq.company (findDisplayName pq.naic category)
  in
    TableRow
      pq.company
      displayName
      ( safeString pq.fRate )
      ( safeString pq.gRate )
      ( safeString pq.nRate )
      pq.naic
      showRowInit
      category
      priority

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

strMaybeDate : Maybe CustomDate -> String
strMaybeDate ccd =
  case ccd of
    Just cd ->
      MyDate.formatRequest cd
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

safeChecked : Maybe Bool -> Attribute msg
safeChecked mb =
  case mb of
    Just b ->
      checked b
    Nothing ->
      checked False

safeCurrencyFloat : Maybe String -> Float
safeCurrencyFloat ss =
  case ss of
    Just s ->
      case String.toFloat (String.replace "$" "" s) of
        Just f ->
          f
        Nothing ->
          0.0
    Nothing ->
      0.0


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
  if model.valid then
    let
      url1 =  "https://enlightnu-quote-api.herokuapp.com/api/plans?"
      url2 = url1
          ++ "zip=" ++ ( stringMaybeInt model.zip.value )
          ++ "&age=" ++ ( stringMaybeInt model.age.value )
          ++ "&county=" ++ ( strCounty model.county )
          ++ "&gender=" ++ model.gender
          ++ "&tobacco=" ++ ( model.tobacco |> boolString )
          ++ "&discounts=" ++ ( model.discounts |> boolString )
          ++ "&date=" ++ ( strMaybeDate model.date )

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
  map5
    PlanQuote
    ( field "company" string )
    ( field "F Rate" ( maybe string ) )
    ( field "G Rate" ( maybe string ) )
    ( field "N Rate" ( maybe string ) )
    ( field "naic" int )


planXDecoder : Decoder (List PlanQuote)
planXDecoder =
  Json.Decode.list planRateDecoder



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


-- PRESETS

findCategory : Int -> RowCategory
findCategory i =
  if List.member i naicCategory.preferred then
    Preferred
  else
    if List.member i naicCategory.nonPreferred then
      NonPreferred
    else
      Outside


findDisplayName : Int -> RowCategory -> Maybe String
findDisplayName i cat =
  case cat of
    Preferred ->
      findDisplayNameUtil i displayNames.preferred
    NonPreferred ->
      findDisplayNameUtil i displayNames.nonPreferred
    Outside ->
      Nothing


findDisplayNameUtil : Int -> (List (Int, String)) -> Maybe String
findDisplayNameUtil i tpls =
  let
    ls = List.filter (\a -> (Tuple.first a) == i) tpls
  in
    case List.head(ls) of
      Just tup ->
        Just <| Tuple.second(tup)
      Nothing ->
        Nothing
