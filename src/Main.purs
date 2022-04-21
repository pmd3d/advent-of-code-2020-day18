module Main
  where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), toUnfoldable, (:))
import Data.List.NonEmpty (NonEmptyList(..), appendFoldable, cons, toList)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Number (fromString)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception.Unsafe (unsafeThrow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (many1, option, try)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.String.Basic (digit)

data Leaf = Number (Maybe Number) | Paren Factor
data Factor = Leaf Leaf | Term Factor Factor | Mult Factor Factor

derive instance genericMyADT :: Generic Factor _

maxDepth :: Int
maxDepth = 999999

convertToMaybeNumber ∷ NonEmptyList Char → Leaf
convertToMaybeNumber xs = Number (fromString $ fromCharArray $ toUnfoldable $ toList xs)

parseLeaf :: Parser String Leaf
parseLeaf = convertToMaybeNumber <$> 
                (appendFoldable <$> many1 digit <*> 
                  (option
                    (NonEmptyList ('.' :| '0' : Nil))
                    (cons <$> char '.' <*> many1 digit)))

-- put in limit so this could compile with circular calls. 
-- couldn't figure out other way with lambdas...
parseLeafLimit ∷ Int -> Parser String Leaf
parseLeafLimit limit | limit > 0 = (try 
              do
                _ <- char '('
                leaf <- Paren <$> parseTop (limit - 1)
                _ <- char ')'
                pure leaf
              )
              <|>
              parseLeaf
parseLeafLimit _ = parseLeaf

parseMult :: Int -> Parser String Factor
parseMult limit | limit > 0 = do
  f1 <- (try $ parseAdd maxDepth) <|> (Leaf <$> parseLeafLimit (limit - 1))
  f2 <- go
  pure $ Mult f1 f2
  where go =  char '*' *> 
              ((try 
                (do
                  f1' <- (try $ parseAdd maxDepth) <|> (Leaf <$> parseLeafLimit (limit - 1))
                  f2' <- try go -- wanted this to be optional...ended up adding another parseAdd...
                  pure $ Mult f1' f2'
                )
              )
              <|> (try $ parseAdd maxDepth)
              <|> (Leaf <$> parseLeafLimit maxDepth))
parseMult _ = unsafeThrow "overflow tree depth"

parseAdd :: Int -> Parser String Factor
parseAdd limit | limit > 0 = do
  t1 <- Leaf <$> (try $ parseLeafLimit (limit - 1))
  t2 <- go
  pure $ Term t1 t2
  where go =  char '+' *> 
                (try (
                do
                  t2' <- Leaf <$> (try $ parseLeafLimit (limit - 1))
                  t3' <- try go
                  pure $ Term t2' t3'
                )
                <|> (Leaf <$> parseLeafLimit (limit - 1)))
parseAdd _ = unsafeThrow "overflow tree depth"

parseTop :: Int -> Parser String Factor
parseTop limit | limit > 0 = (try $ parseMult (limit - 1)) <|> (parseAdd (limit - 1))
parseTop _ = unsafeThrow "overflow tree depth"

eval :: Factor -> Number 
eval e = 
  case e of 
    Mult first second ->
      n1 * n2 where
        n1 = eval first
        n2 = eval second
    Term first second ->
      n1 + n2 where
        n1 = eval first
        n2 = eval second
    Leaf (Number (Just n)) -> n
    Leaf (Number Nothing) -> unsafeThrow "evaluation error of number"
    Leaf (Paren p) -> eval p

showFactor ∷ Factor → String
showFactor f = 
  case f of 
    Mult first second ->
      "(F " <> s1 <> "*" <> s2 <> ")" where
        s1 = showFactor first
        s2 = showFactor second
    Term first second ->
      "(T " <> t1 <> "+" <> t2 <> ")" where
        t1 = showFactor first
        t2 = showFactor second
    Leaf (Number (Just n)) -> show n
    Leaf (Number Nothing) -> unsafeThrow "evaluation error of number"
    Leaf (Paren p) -> "(" <> showFactor p <> ")"

run :: String -> Maybe Number
run s = 
  do
    case runParser s (parseTop maxDepth) of
      Right f -> Just $ eval f
      Left _ -> Nothing

main :: Effect Unit
main = do
  s <- readTextFile UTF8 "AoCInput2020Day18.txt"
  let text = lines $ replaceAll (Pattern " ") (Replacement "") s
  log $ show $ sum <$> traverse run text 
