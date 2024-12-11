module DayTwo where

import Prelude

pair :: [a] -> [(a, a)]
pair [] = []
pair [_] = []
pair [x, y] = [(x, y)]
pair (x:y:xs) = (x, y) : pair (y:xs)

one_to_three_in_order :: (Enum a, Num a, Ord a) => (a -> a -> Bool) -> (a, a) -> Bool
one_to_three_in_order f (x, y) = abs (x - y) `elem` [1 .. 3] && f x y

drop_report :: (Enum a, Num a, Ord a) => [a] -> [[a]]
drop_report [] = []
drop_report (x:xs) = xs : map (x:) (drop_report xs)

is_safe_asc :: (Enum a, Num a, Ord a) => [a] -> Bool
is_safe_asc xs = all (one_to_three_in_order (<)) (pair xs)

is_safe_desc :: (Enum a, Num a, Ord a) => [a] -> Bool
is_safe_desc xs = all (one_to_three_in_order (>)) (pair xs)

is_safe :: (Enum a, Num a, Ord a) => [a] -> Bool
is_safe xs = is_safe_asc xs || is_safe_desc xs

is_safe2 :: (Enum a, Num a, Ord a) => [a] -> Bool
is_safe2 xs = any is_safe (drop_report xs)

check :: (Enum a, Eq a, Num a, Ord a) => [[a]] -> ([a] -> Bool) -> [Bool]
check xs f = filter id $ map f xs

calculate_safe :: (Enum a, Eq a, Num a, Ord a) => [[a]] -> ([a] -> Bool) -> Int
calculate_safe xs f = length $ check xs f

parse :: String -> [[Int]]
parse = map (map read . words) . lines

day_two :: IO ()
day_two = do
  content <- readFile "inputs/d2.txt" :: IO String
  let read_lines = parse content
  let safe = calculate_safe read_lines is_safe
  print "PART I:"
  print safe
  let safe_damp = calculate_safe read_lines is_safe2
  print "PART II:"
  print safe_damp
