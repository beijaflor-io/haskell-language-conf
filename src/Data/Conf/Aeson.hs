{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
module Data.Conf.Aeson
  where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Key (fromText, toText)
import           Data.Aeson.KeyMap (toHashMap)
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Scientific       as Scientific
import           Data.String
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector

import           Data.Conf.PrettyPrint
import           Data.Conf.Types

fromToJSON :: ToJSON a => a -> Result Conf
fromToJSON = fromJSON . toJSON

instance {-# OVERLAPPING #-} FromJSON Conf where
    parseJSON (Object obj) = do
        let oList = HashMap.toList $ toHashMap obj
        forM oList $ \(k, v) ->
            case v of
                o@(Object _) -> do
                    cs <- parseJSON o
                    return $ ConfStatementBlock (Block (Text.words $ toText k) cs)
                (Array vs) -> return $
                    ConfStatementExpression (Expression (toText k) (map toExpressionValue (Vector.toList vs))) (toInlineComment (last $ Vector.toList vs))
                value -> return $
                    ConfStatementExpression (Expression (toText k) [toExpressionValue value]) (toInlineComment value)
          where
            toExpressionValue (Number oc) =
                case Scientific.floatingOrInteger oc of
                    Left f -> fromString (show (f :: Double))
                    Right i -> fromString (show (i :: Integer))
            toExpressionValue (String oc) = oc
            toExpressionValue (Bool b) = if b then "true" else "false"
            toExpressionValue _ = error "Invalid type"
            toInlineComment (String s) = case Text.break (=='#') s of
                                           (_, "") -> Nothing
                                           (_, comm) -> Just $ Comment $ Text.cons ' ' $ Text.tail comm
            toInlineComment _ = Nothing

    parseJSON invalid = typeMismatch "Conf" invalid

instance {-# OVERLAPPING #-} ToJSON Conf where
    toJSON cs = object ps
      where
        ps = concatMap toPair cs
        toPair (ConfStatementExpression (Expression e [v]) Nothing) = [ fromText e .= String v ]
        toPair (ConfStatementExpression (Expression e [v]) (Just (Comment c))) = [ fromText e .= (String $ v <> " #" <> c) ]
        toPair (ConfStatementExpression (Expression e vs) Nothing) = [ fromText e .= toJSON vs ]
        toPair (ConfStatementExpression (Expression e vs) (Just (Comment c))) = [ fromText e .= (toJSON $ vs <> [" #", c]) ]
        toPair (ConfStatementBlock (Block [k] css)) = [ fromText k .= toJSON css ]
        toPair (ConfStatementBlock (Block ks css)) = [ fromText (Text.unwords ks) .= toJSON css ]
        toPair ConfStatementEmptyLine = []
        toPair (ConfStatementComment _) = []
