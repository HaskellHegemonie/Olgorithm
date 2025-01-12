{-# LANGUAGE LambdaCase #-}
module Morse where
import Data.List.Split
import Text.Printf
import Data.List
import Data.Function
import Control.Lens
import Data.Char
import Data.Map qualified as M

-- Provided by Emacs
table = [("a", ".-")
        ,("b", "-...")
        ,("c", "-.-.")
        ,("d", "-..")
        ,("e", ".")
        ,("f", "..-.")
        ,("g", "--.")
        ,("h", "....")
        ,("i", "..")
        ,("j", ".---")
        ,("k", "-.-")
        ,("l", ".-..")
        ,("m", "--")
        ,("n", "-.")
        ,("o", "---")
        ,("p", ".--.")
        ,("q", "--.-")
        ,("r", ".-.")
        ,("s", "...")
        ,("t", "-")
        ,("u", "..-")
        ,("v", "...-")
        ,("w", ".--")
        ,("x", "-..-")
        ,("y", "-.--")
        ,("z", "--..")
        ,("=", "-...-")
        ,("?", "..--..")
        ,("/", "-..-.")
        ,(",", "--..--")
        ,(".", ".-.-.-")
        ,(":", "---...")
        ,("'", ".----.")
        ,("-", "-....-")
        ,("(", "-.--.-")
        ,(")", "-.--.-")
        ,("0", "-----")
        ,("1", ".----")
        ,("2", "..---")
        ,("3", "...--")
        ,("4", "....-")
        ,("5", ".....")
        ,("6", "-....")
        ,("7", "--...")
        ,("8", "---..")
        ,("9", "----.")
        ,("ä", ".-.-")
        ,("æ", ".-.-")
        ,("á", ".--.-")
        ,("å", ".--.-")
        ,("ß", ".../...")
        ,("é", "..-..")
        ,("ñ", "--.--")
        ,("ö", "---.")
        ,("ø", "---.")
        ,("ü", "..--")
        ,("@", ".--.-.")
        ,(" ", " ")
        ]
maptable = M.fromList table
unmaptable = M.fromList $ map (uncurry (flip (,))) table

morse xs = xs & mapped %~ (\x -> maptable M.! [toLower x]) & intercalateBy "/"
  where
    intercalateBy _ [] = []
    intercalateBy _ [x] = x
    intercalateBy sep (x:y:xs) = if any (== " ") [x, y] then x <> intercalateBy sep (y:xs) else x <> sep <> intercalateBy sep (y:xs)

morsd = concatMap (unmaptable M.!) . filter (not . null) . splitMorse
  where
    splitMorse [] = [[]]
    splitMorse [x] = if x == '/' then [[]] else [[x]]
    splitMorse (x:xs) =
      case x of
        '/' -> [] : sub
        ' ' -> [] : " " : sub
        _ -> (x : h) : t
      where
        sub@(h:t) = splitMorse xs
