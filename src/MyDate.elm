module MyDate exposing (CustomDate, addMonth, toString, formatRequest)

import Time exposing (Month(..), utc)
import String

type alias CustomDate =
  { month : Month
  , year : Int
  }

addMonth : Int -> CustomDate -> CustomDate
addMonth i cd =
  let
    di = (monthInt cd.month) + i
    newMonth = intMonth <| modBy 12 di
    newYear = if cd.month ==  Dec then
                  if i > 0 then
                      cd.year + 1
                  else
                      cd.year
              else
                  cd.year + ( (di - 1) // 12 )
  in
    CustomDate newMonth newYear

formatRequest : CustomDate -> String
formatRequest cd =
  let
    mi = monthInt cd.month
    ms =  if mi < 10 then
             if mi == 0 then
                 "12"
             else
                 "0" ++ String.fromInt mi
          else
            String.fromInt mi
    ys = String.fromInt cd.year
  in
    ys ++ "-" ++ ms ++ "-01"

toString : CustomDate -> String
toString cd =
  let
    monthString = case cd.month of
      Jan -> "January"
      Feb -> "February"
      Mar -> "March"
      Apr -> "April"
      May -> "May"
      Jun -> "June"
      Jul -> "July"
      Aug -> "August"
      Sep -> "September"
      Oct -> "October"
      Nov -> "November"
      Dec -> "December"
    yearString = String.fromInt cd.year
  in
    monthString ++ " "  ++ yearString


monthInt : Month -> Int
monthInt m =
  case m of
    Jan -> 1
    Feb -> 2
    Mar -> 3
    Apr -> 4
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 0

intMonth : Int -> Month
intMonth i =
  case i of
    1 -> Jan
    2 -> Feb
    3 -> Mar
    4 -> Apr
    5 -> May
    6 -> Jun
    7 -> Jul
    8 -> Aug
    9 -> Sep
    10 -> Oct
    11 -> Nov
    0 -> Dec
    _ -> Jan
