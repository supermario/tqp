

module Car
  (carOption)
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

carOption :: Parsec Dec String [OptionCar]
carOption = do
    optionNumber <- numbers <* space
    carSegments <- someTill carSegment carFooter
    space
    return carSegments

-- **************************************************************
-- CAR OPTION 1
--
-- Thrifty Car Rental
--
-- PICK UP:
--  Wed May 31 2017 at 10:39 PM
--  IN TERMINAL
--  Kingsford Smith (SYD)
--  Sydney, NS, Australia
--
-- DROP OFF:
--  Wed May 31 2017 at 10:44 PM
--
-- CAR TYPE: Economy
--
-- Daily Rate: 44.98 AUD
-- Approximate total: 65.31 AUD
carSegment = do
    supplierName <- untilEol
    space
    string "PICK UP:"
    startDatetime <- space >> untilEol
    startLoc1 <- space >> untilEol
    startLoc2 <- space >> untilEol
    startLoc3 <- space >> untilEol
    space >> string "DROP OFF:"
    endDatetime <- space >> untilEol
    carType <- space >> string "CAR TYPE: " >> untilEol
    return
        OptionCar
        { supplierName = supplierName
        , startDatetime = startDatetime
        , startLoc1 = startLoc1
        , startLoc2 = startLoc2
        , startLoc3 = startLoc3
        , endDatetime = endDatetime
        , carType = carType
        }

carFooter = do
    rate <- space >> string "Daily Rate: " >> untilEol
    total <- space >> string "Approximate total: " >> untilEol
    return ()

data OptionCar = OptionCar
    { supplierName  :: String
    , startDatetime :: String
    , startLoc1     :: String
    , startLoc2     :: String
    , startLoc3     :: String
    , endDatetime   :: String
    , carType       :: String
    } deriving (((Show)))
