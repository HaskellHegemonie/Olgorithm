{-# LANGUAGE LambdaCase #-}
module Hubsi where


hubse = map (\case 'a' -> 'o'; 'A' -> 'O'; x -> x)
hubsd = map (\case 'o' -> 'a'; 'O' -> 'A'; x -> x)
