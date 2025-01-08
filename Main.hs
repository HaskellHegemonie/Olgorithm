module Main where
import Text.Printf
import Options.Applicative
import Data.String
import Control.Monad
import B32 qualified as B32
import B64 qualified as B64

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
  <|> subparser (command "eb64"  (info (inpEBase64 <$> argument str idm) $ progDesc "Base64 Encode"))
  -- <|> subparser (command "db64"  (info (inpEBase64 <$> argument str idm) $ progDesc "Base64 Decode"))


p = printf "\"%s\"\n"
inpId :: String -> IO ()
inpId = p
inpReverse = p . reverse
inpEBase32 = p . B32.encode
inpDBase32 = p . B32.decode

inpEBase64 = p . B64.encode
-- inpDBase64 = p . B64.decode
