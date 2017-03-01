{-# LANGUAGE RecordWildCards #-}

module Hotel
  (hotelOption)
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

hotelOption :: Parsec Dec String [OptionHotel]
hotelOption = do
    optionNumber <- numbers <* space
    hotelSegments <- someTill hotelSegment hotelFooter
    space
    return hotelSegments

-- HOLIDAY INN DARLING HARBOUR
--
--  HOLIDAY INN DARLING HARBOUR
--  68 HARBOUR ST
--  Sydney NS, AU
--
--  CHECK-IN: Wed May 31 2017
--  CHECK-OUT: Thu Jun 01 2017
--
-- ADV PURCHASE BKFST SPECIAL SAVINGS ADVANCE PURCHASE. INCLUDES BREAKFAST FOR ONE OR TWO DELUXE ROOM WHEN YOU ARRIVE WE WILL DO OUR BEST BED: N/A MEAL: B         VIEW: N/ABOOKINGS ARE NON-REFUNDABLE
--
-- Price per night: 316.00 AUD
-- Approximate total: 320.74 AUD
hotelSegment = do
    name <- untilEol
    space
    otherName <- untilEol
    address1 <- untilEol
    address2 <- untilEol
    checkInDate <- space >> string "CHECK-IN: " >> untilEol
    checkOutDate <- space >> string "CHECK-OUT: " >> untilEol
    space
    sellNotes <- untilEol
    space
    return OptionHotel {..}

hotelFooter = do
    string "Price" >> untilEol
    string "Approximate" >> untilEol

data OptionHotel = OptionHotel
    { name         :: String
    , otherName    :: String
    , address1     :: String
    , address2     :: String
    , checkInDate  :: String
    , checkOutDate :: String
    , sellNotes    :: String
    } deriving (Show)
