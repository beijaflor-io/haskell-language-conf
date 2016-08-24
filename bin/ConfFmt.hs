{-# LANGUAGE RecordWildCards #-}
import           Control.Monad
import qualified Data.Conf                 as Conf
import qualified Data.Text.IO              as Text (hGetContents, readFile)
import           Options.Applicative
import           Options.Applicative.Types
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure)
import           System.IO
import qualified Text.Megaparsec           as Megaparsec

data ConfFmtOptions
    = ConfFmtOptions { cfoInplace :: Bool
                     , cfoOutput  :: Maybe FilePath
                     , cfoInput   :: Maybe FilePath
                     }

options :: Parser ConfFmtOptions
options = ConfFmtOptions
    <$> cfoInplaceAP
    <*> cfoOutputAP
    <*> cfoInputAP

cfoInputAP = optional (argument str
    (metavar "INPUTFILE"
    <> help "The input file (omission will cause `conffmt` to use stdin)"))

cfoOutputAP = optional (option str (short 'o' <> long "output"))
cfoInplaceAP = switch (long "inplace"
    <> short 'i'
    <> help "Format the FILE inplace, replacing it's contents")

main :: IO ()
main = do
    ConfFmtOptions{..} <- execParser (info (helper <*> options)
                                   ( fullDesc
                                     <> progDesc "Format a .conf FILE"
                                     <> header "conffmt - Conf formatter using Haskell language-conf"
                                   ))
    (contents, fp) <- case cfoInput of
        Just fp ->
            (,) <$> Text.readFile fp <*> pure fp
        Nothing ->
            (,) <$> Text.hGetContents stdin <*> pure "<stdin>"
    case Megaparsec.parse Conf.conf fp contents of
        Left err -> hPrint stderr err
        Right ast -> do
            let output = show (Conf.pPrintConf ast)
            case (cfoInplace, cfoOutput) of
                (_, Just outputFp) -> writeFile outputFp output
                (True, _) -> writeFile fp output
                _ -> putStr output
