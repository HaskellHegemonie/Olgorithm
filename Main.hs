module Main where
import Text.Printf
import Options.Applicative
import Data.String
import Control.Monad
import B32 qualified
import B64 qualified
import Hex qualified

main = join $ execParser (info commandParser $ progDesc "Olgorithm")

commandParser :: Parser (IO ())
commandParser =
  subparser (command "id" (info (inpId <$> argument str idm) $ progDesc "Id"))

  <|> subparser (command "rev"  (info (inpReverse <$> argument str idm) $ progDesc "Reverse"))

  <|> subparser (command "revd"  (info (inpReverse <$> argument str idm) $ progDesc "Decodes Reversed String 🗿"))

  <|> subparser (command "hexe"  (info (inpHexe <$> argument str idm) $ progDesc "Jan ist eine Hexe"))
  <|> subparser (command "hexd"  (info (inpHexd <$> argument str idm) $ progDesc "Decode Hex String"))

  <|> subparser (command "b32e"  (info (inpEBase32 <$> argument str idm) $ progDesc "Base32 Encode"))
  <|> subparser (command "b32d"  (info (inpDBase32 <$> argument str idm) $ progDesc "Base32 Decode"))

  <|> subparser (command "b64e"  (info (inpEBase64 <$> argument str idm) $ progDesc "Base64 Encode"))
  -- <|> subparser (command "db64"  (info (inpEBase64 <$> argument str idm) $ progDesc "Base64 Decode"))


p = printf "\"%s\"\n"

inpId :: String -> IO ()
inpId = p

inpReverse = p . reverse

inpHexe = p . Hex.hexe
inpHexd = p . Hex.hexd

inpEBase32 = p . B32.encode
inpDBase32 = p . B32.decode

inpEBase64 = p . B64.encode
-- inpDBase64 = p . B64.decode
