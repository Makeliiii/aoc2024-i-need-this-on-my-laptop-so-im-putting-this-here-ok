module Driver (driver, parse_args) where

import DayOne (day_one)
import DayTwo (day_two)

import Data.Char (toLower)

data Day = First | Second | DayError

driver :: Maybe Day -> IO ()
driver (Just First) = day_one
driver (Just Second) = day_two
driver (Just DayError) = print "No such day :)"
driver Nothing = print "Give args :)"

parse_args :: [String] -> Maybe Day
parse_args [] = Nothing
parse_args xs = Just $ encode_day $ head xs

encode_day :: String -> Day
encode_day day
  | map toLower day == "first" = First
  | map toLower day == "second" = Second
  | otherwise = DayError
