module Lib
  (someFunc)
  where

import           Car                    (carOption)
import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              as T
import           Data.Time
import           Data.Time.Format
import           Flight                 (flightOption)
import           Helpers
import           Hotel                  (hotelOption)
import           Prelude                hiding (until)
import           Text.Megaparsec
import           Text.Megaparsec.String
import           Text.Show.Pretty       (ppShow)

someFunc :: IO ()
someFunc = do
    fileContents <- readFile "fixtures/sampleAllSegments.txt"
    case parse quotes "" fileContents of
        Left e  -> print e
        Right d -> mapM_ putStrLn d

quotes :: Parsec Dec String [String]
quotes = do
    string "Trip Quote" >> untilEol >> space
    some optionParser

optionParser = do
    some (string "*") >> space
    segmentType <- until " OPTION "
    case segmentType of
        "FLIGHT" -> ppShow <$> flightOption
        "HOTEL"  -> ppShow <$> hotelOption
        "CAR"    -> ppShow <$> carOption
        _        -> return "failure"
