-- | Hand-written lexer for MiniML. Supports nested @(* ... *)@ comments.
module MiniML.Lexer
  ( Tok (..)
  , Token (..)
  , LexError (..)
  , lexML
  , keywords
  , showTok
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf)
import MiniML.Syntax (Pos (..))

data Tok
  = TInt Integer
  | TStr String
  | TIdent String
  | TKw String
  | TSym String
  | TEOF
  deriving (Eq, Show)

data Token = Token { tokPos :: !Pos, tokVal :: !Tok }
  deriving (Eq, Show)

data LexError = LexError Pos String
  deriving (Eq, Show)

keywords :: [String]
keywords = ["let", "rec", "in", "fun", "if", "then", "else", "match", "with", "true", "false"]

-- Two-character symbols must come before their one-character prefixes.
symbols :: [String]
symbols =
  [ "->", "::", "<=", ">=", "<>", "&&", "||", ";;"
  , "(", ")", "[", "]", ",", ";", "|", "=", "<", ">"
  , "+", "-", "*", "/", "%", "^", "@"
  ]

showTok :: Tok -> String
showTok t = case t of
  TInt n -> "integer " ++ show n
  TStr _ -> "a string literal"
  TIdent x -> "identifier '" ++ x ++ "'"
  TKw k -> "keyword '" ++ k ++ "'"
  TSym s -> "'" ++ s ++ "'"
  TEOF -> "end of input"

lexML :: String -> Either LexError [Token]
lexML = go (Pos 1 1)
  where
    go pos [] = Right [Token pos TEOF]
    go pos s@(c : cs)
      | c == '\n' = go (newline pos) cs
      | isSpace c = go (adv pos 1) cs
      | "(*" `isPrefixOf` s = comment pos (adv pos 2) (1 :: Int) (drop 2 s)
      | isDigit c =
          let (ds, rest) = span isDigit s
          in if not (null rest) && isIdentStart (head rest)
               then Left (LexError pos ("malformed number '" ++ ds ++ takeWhile isIdentChar rest ++ "'"))
               else (Token pos (TInt (read ds)) :) <$> go (adv pos (length ds)) rest
      | c == '"' = str pos (adv pos 1) "" cs
      | isIdentStart c =
          let (w, rest) = span isIdentChar s
              tok | w == "_" = TSym "_"
                  | w `elem` keywords = TKw w
                  | otherwise = TIdent w
          in (Token pos tok :) <$> go (adv pos (length w)) rest
      | otherwise =
          case filter (`isPrefixOf` s) symbols of
            (sym : _) -> (Token pos (TSym sym) :) <$> go (adv pos (length sym)) (drop (length sym) s)
            [] -> Left (LexError pos ("unexpected character " ++ show c))

    comment start _ _ [] = Left (LexError start "unterminated comment")
    comment start pos depth s@(c : cs)
      | "*)" `isPrefixOf` s =
          if depth == 1 then go (adv pos 2) (drop 2 s) else comment start (adv pos 2) (depth - 1) (drop 2 s)
      | "(*" `isPrefixOf` s = comment start (adv pos 2) (depth + 1) (drop 2 s)
      | c == '\n' = comment start (newline pos) depth cs
      | otherwise = comment start (adv pos 1) depth cs

    str start _ _ [] = Left (LexError start "unterminated string literal")
    str start pos acc (c : cs) = case c of
      '"' -> (Token start (TStr (reverse acc)) :) <$> go (adv pos 1) cs
      '\n' -> Left (LexError start "newline in string literal")
      '\\' -> case cs of
        (e : rest) | Just ch <- lookup e escapes -> str start (adv pos 2) (ch : acc) rest
        (e : _) -> Left (LexError pos ("unknown escape sequence \\" ++ [e]))
        [] -> Left (LexError start "unterminated string literal")
      _ -> str start (adv pos 1) (c : acc) cs

    escapes = [('n', '\n'), ('t', '\t'), ('\\', '\\'), ('"', '"')]
    adv (Pos l col) n = Pos l (col + n)
    newline (Pos l _) = Pos (l + 1) 1

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''
