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
            mv <- case takeExtension fp of
                ".yaml" -> Yaml.decodeFile fp
                ".json" -> Aeson.decode <$> ByteString.readFile fp
            case mv of
                Just v -> do
                    let conf :: Aeson.Result Conf.Conf
                        conf = Aeson.fromJSON v
                    writeFile output $ show $ case conf of
                        Aeson.Success c -> Conf.pPrint c
                        Aeson.Error e -> error ("Tranformation failure: " <> e)
                Nothing -> error ("Parse failure " <> fp)
        _ -> error "Usage: toconf <inputfile> <outputfile>"
