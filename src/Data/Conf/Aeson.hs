{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Conf.Aeson
  where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Scientific       as Scientific
import           Data.String
import qualified Data.Text             as Text
import qualified Data.Vector           as Vector

import           Data.Conf.PrettyPrint
import           Data.Conf.Types

fromToJSON :: ToJSON a => a -> Result Conf
fromToJSON = fromJSON . toJSON

instance FromJSON Conf where
    parseJSON (Object obj) = do
        let oList = HashMap.toList obj
        forM oList $ \(k, v) ->
            case v of
                o@(Object _) -> do
                    cs <- parseJSON o
                    return $ ConfStatementBlock (Block (Text.words k) cs)
                (Array vs) -> return $
                    ConfStatementExpression (Expression k (map toExpressionValue (Vector.toList vs)))
                value -> return $
                    ConfStatementExpression (Expression k [toExpressionValue value])
          where
            toExpressionValue (Number oc) =
                case Scientific.floatingOrInteger oc of
                    Left f -> fromString (show (f :: Double))
                    Right i -> fromString (show (i :: Integer))
            toExpressionValue (String oc) = oc
            toExpressionValue (Bool b) = if b then "true" else "false"
            toExpressionValue _ = error "Invalid type"

    parseJSON invalid = typeMismatch "Conf" invalid

instance ToJSON Conf where
    toJSON cs = object ps
      where
        ps = concatMap toPair cs
        toPair (ConfStatementExpression (Expression e [v])) = [ e .= String v ]
        toPair (ConfStatementExpression (Expression e vs)) = [ e .= toJSON vs ]
        toPair (ConfStatementBlock (Block [k] css)) = [ k .= toJSON css ]
        toPair (ConfStatementBlock (Block ks css)) = [ Text.unwords ks .= toJSON css ]
        toPair ConfStatementEmptyLine = []
        toPair (ConfStatementComment _) = []
