module Flight
  (flightOption)
  where

import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              as T
import           Data.Time
import           Data.Time.Format
import           Helpers
import           Prelude                hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.String

flightOption :: Parsec Dec String [OptionFlight]
flightOption = do
    optionNumber <- numbers <* space
    flightSegments <- someTill flightSegment footer
    space
    return flightSegments

footer = do
    string "Price" >> untilEol
    string "Total" >> untilEol

flightSegment = do
    citiesHeaderP
    space
    journeyTimeP
    space
    (platingCarrierName,platingCarrierCode,flightNumber) <- platingCarrier
    space
    (startAirportName,startCityName,startAirportCode) <- string "FROM: " >> airport
    space
    (endAirportName,endCityName,endAirportCode) <- string "TO: " >> airport
    space
    startDatetime <- string "DEPARTS: " >> longDate
    space
    endDatetime <- string "ARRIVES: " >> longDate
    space
    (cabinClassName,cabinClassCode,hours,minutes) <- cabinAndDuration
    space
    (operatingCarrierName,aircraftName) <- operatorAndAircraft
    space
    return
        OptionFlight
        { startCityName = startCityName
        , startAirportCode = startAirportCode
        , startAirportName = startAirportName
        , startDatetime = startDatetime
        , endCityName = endCityName
        , endAirportCode = endAirportCode
        , endAirportName = endAirportName
        , endDatetime = endDatetime
        , duration = hours <> "h " <> minutes <> "m"
        , platingCarrierName = platingCarrierName
        , platingCarrierCode = platingCarrierCode
        , flightNumber = platingCarrierCode <> flightNumber
        , operatingCarrierName = operatingCarrierName
        , cabinClassName = cabinClassName
        , cabinClassCode = cabinClassCode
        , aircraftName = aircraftName
        }

-- MELBOURNE > SYDNEY
citiesHeaderP
    :: Parsec Dec String (String, String)
citiesHeaderP = do
    fromName <- until " > "
    toName <- untilEol
    return (fromName, toName)

-- Total journey time:  1h 25m
journeyTimeP
    :: Parsec Dec String (String, String)
journeyTimeP = space >> string "Total journey time:" >> space >> hoursMinutes

-- Jetstar Airways Pty Limited - JQ 608
platingCarrier = do
    space
    carrierName <- until " - "
    carrierCode <- until " "
    space
    flightNumber <- untilEol
    return (carrierName, carrierCode, flightNumber)

-- Sun Apr 30 2017 at 07:00 PM
longDate
    :: Parsec Dec String (Maybe UTCTime)
longDate = do
    str <- untilEol
    return $ parseTimeM True defaultTimeLocale "%a %b %e %Y at %H:%M %p" str

-- CABIN CLASS: Economy (K), DURATION:  1h 25m
cabinAndDuration = do
    className <- string "CABIN CLASS: " >> until " ("
    classCode <- until "), "
    (hours,minutes) <- string "DURATION:" >> space >> hoursMinutes
    return (className, classCode, hours, minutes)

-- OPERATED BY: Jetstar Airways Pty Limited, AIRCRAFT: Airbus Industrie A320-100/200
operatorAndAircraft = do
    operatorName <- string "OPERATED BY: " >> until ", "
    aircraftName <- string "AIRCRAFT: " >> untilEol
    return (operatorName, aircraftName)

-- Kingsford Smith, Sydney (SYD)
airport = do
    airportName <- until ", "
    cityName <- until " ("
    airportCode <- until ")"
    return (airportName, cityName, airportCode)

-- 1h 34m
hoursMinutes = do
    hours <- numbers <* string "h"
    minutes <- space *> numbers <* string "m"
    return (hours, minutes)

data OptionFlight = OptionFlight
    { startCityName        :: String
    , startAirportCode     :: String
    , startAirportName     :: String
    , startDatetime        :: Maybe UTCTime
    , endCityName          :: String
    , endAirportCode       :: String
    , endAirportName       :: String
    , endDatetime          :: Maybe UTCTime
    , duration             :: String
    , platingCarrierName   :: String
    , platingCarrierCode   :: String
    , flightNumber         :: String
    , operatingCarrierName :: String
    , cabinClassName       :: String
    , cabinClassCode       :: String
    , aircraftName         :: String
    } deriving ((Show))
