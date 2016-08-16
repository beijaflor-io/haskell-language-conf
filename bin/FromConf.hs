import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Conf            as Conf
import           Data.Monoid
import qualified Data.Text.IO         as Text
import qualified Data.Yaml            as Yaml
import           System.Environment
import           System.FilePath
import qualified Text.Megaparsec      as Megaparsec

main :: IO ()
main = do
    as <- getArgs
    case as of
        (fp:output:_) -> do
            fcontents <- Text.readFile fp
            let conf = case Megaparsec.parse Conf.conf fp fcontents of
                    Right c -> c
                    Left e -> error ("Failed to parse " <> fp <> "\n" <> show e)
                confv = Aeson.toJSON conf
            case takeExtension output of
                ".json" -> ByteString.writeFile output (Aeson.encode confv)
                ".yaml" -> Yaml.encodeFile output confv
                _ -> error "Unsupported output format"
        _ -> error "Usage: fromconf <inputfile> <outputfile>"
