module Main where
import Text.Printf
import Options.Applicative
import Data.String
import Control.Monad
import B32

data Command = Id String
             | Reverse String
             | EBase32 String
             | DBase32 String

main = join $ execParser (info commandParser $ progDesc "Olgorithm")

commandParser :: Parser (IO ())
commandParser =
  subparser (command "id" (info (inpId <$> argument str idm) $ progDesc "Id"))
  <|> subparser (command "rev"  (info (inpReverse <$> argument str idm) $ progDesc "Reverse"))
  <|> subparser (command "eb32"  (info (inpEBase32 <$> argument str idm) $ progDesc "Base32 Encode"))
  <|> subparser (command "db32"  (info (inpDBase32 <$> argument str idm) $ progDesc "Base32 Decode"))


p = printf "\"%s\"\n"
inpId :: String -> IO ()
inpId = p
inpReverse = p . reverse
inpEBase32 = p . encode
inpDBase32 = p . decode
