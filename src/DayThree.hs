{-# LANGUAGE OverloadedStrings #-}

module DayThree (day_three) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Text.IO as TIO
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Limiter = LeftParen | Comma | RightParen

data Command = Mul deriving (Eq, Show)

data Multiplication = Multiplication {
  x :: Integer,
  y :: Integer
} | Enabled | Disabled deriving (Eq, Show)

mul :: Parser Command
mul = Mul <$ string "mul"

left_paren :: Parser Limiter
left_paren = LeftParen <$ char '('

right_paren :: Parser Limiter
right_paren = RightParen <$ char ')'

comma :: Parser Limiter
comma = Comma <$ char ','

integer :: Parser Integer
integer = L.decimal

get_x :: Parser Integer
get_x = mul *> left_paren *> integer

get_y :: Parser Integer
get_y = comma *> integer <* right_paren

multiplication :: Parser Multiplication
multiplication = choice 
  [ Multiplication <$> get_x <*> get_y,
    enable,
    disable
  ]

enable :: Parser Multiplication
enable = Enabled <$ string "do()"

disable :: Parser Multiplication
disable = Disabled <$ string "don't()"

parser :: Parser [Multiplication]
parser = catMaybes <$> Text.Megaparsec.some (choose_multiplication <* optional eol) <* eof
  where
    choose_multiplication = choice
      [ Just <$> try multiplication,
        Nothing <$ anySingle
      ]

multiply :: Multiplication -> Maybe Integer
multiply (Multiplication right left) = Just (right * left)
multiply Disabled = Nothing
multiply Enabled = Nothing

remove_disabled_multiplications :: [Multiplication] -> [Multiplication]
remove_disabled_multiplications = remove_while True
  where
    remove_while :: Bool -> [Multiplication] -> [Multiplication]
    remove_while _ [] = []
    remove_while is_enabled (z:zs)
      | is_enabled = case z of 
          Enabled -> remove_while True zs
          Disabled -> remove_while False zs
          (Multiplication _ _) -> z : remove_while is_enabled zs
      | not is_enabled = case z of
          Enabled -> remove_while True zs
          Disabled -> remove_while False zs
          (Multiplication _ _) -> remove_while is_enabled zs


day_three :: IO ()
day_three = do
  content <- TIO.readFile "inputs/d3.txt" :: IO Text
  let part_one = "PART I:" :: String
  print part_one
  case parse parser "" content of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right mults -> print (sum $ mapMaybe multiply mults)
  let part_two = "PART II:" :: String
  print part_two
  case parse parser "" content of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right mults -> print (sum $ mapMaybe multiply (remove_disabled_multiplications mults))

