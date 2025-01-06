{-# LANGUAGE MultiWayIf #-}
module B32 where
import Data.Array
import Numeric
import Data.List.Split
import Data.Char
import Data.Ix
import Data.Maybe


n :: Num a => a -> a
n = negate

(padding, paddingStr) = (32, "=")

pad f padding x xs = case f of
  Front -> pad <> xs
  Back -> xs <> pad
  where
    l = length xs
    dist = max 0 $ x + n l
    pad = replicate dist padding

data Padding = Front | Back 
encode :: String -> String
encode = concatMap convert
  . chunksOf 8
  . map (pad Back '0' 5)
  . chunksOf 5
  . concatMap (pad Front '0' 8 . flip showBin "" . fromEnum) 
  where
    table = listArray (0, 32) $ ['A'..'Z'] <> ['2'..'7'] <> paddingStr
    convert xs = concatMap (map ((table !) . fst) . readBin) $ (pad Back (showBin padding "") 8) xs
      where
        dist = max 0 $ length xs + n 8


decode :: String -> String
decode str = test
  where
    test = concatMap fun $ chunksOf 8 $ concatMap (pad Front '0' 5 . (flip showBin "") . fromJust . table) $ filter (/= '=') str
    fun x = map (toEnum . fst) $ readBin x
    table x = let x' = fromEnum x in if
      | inRange (fromEnum 'A', fromEnum 'Z') x' -> Just $ x' - fromEnum 'A'
      | inRange (fromEnum '2', fromEnum '7') x' -> Just $ x' + n (fromEnum '2') + 26
      | otherwise -> Nothing
