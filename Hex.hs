module Hex where
import Numeric
import Data.List.Split

n = negate
pad f padding x xs = case f of
  Front -> pad <> xs
  Back -> xs <> pad
  where
    l = length xs
    dist = max 0 $ x + n l
    pad = replicate dist padding

data Padding = Front | Back

hexe :: String -> String
hexe = concatMap (pad Front '0' 2 . flip showHex "" . fromEnum)

hexd :: String -> String
hexd = map (toEnum . head . map fst . readHex) . chunksOf 2
