{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Conf.Types
  where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Scientific     as Scientific
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector

type Conf = [ConfStatement]

data ConfStatement = ConfStatementComment Comment
                   | ConfStatementBlock Block
                   | ConfStatementEmptyLine
                   -- ^ We store empty lines while parsing so we can
                   -- reconstruct the document when pretty-printing
                   | ConfStatementExpression Expression
  deriving(Eq, Show)

data Block = Block [Text] [ConfStatement]
  deriving(Eq, Show)

data Comment = Comment Text
  deriving(Eq, Show)

data Expression = Expression Text [Text]
  deriving(Eq, Show)

instance FromJSON Conf where
    parseJSON (Object o) = do
        let oList = HashMap.toList o
        forM oList $ \(k, v) ->
            case v of
                o@(Object _) -> do
                    cs <- parseJSON o
                    return $ ConfStatementBlock (Block [k] cs)
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

    parseJSON invalid = typeMismatch "Conf" invalid

instance ToJSON Conf where
    toJSON cs = object ps
      where
        ps = concatMap toPair cs
        toPair (ConfStatementExpression (Expression e [v])) = [ e .= String v ]
        toPair (ConfStatementExpression (Expression e vs)) = [ e .= toJSON vs ]
        toPair (ConfStatementBlock (Block [k] css)) = [ k .= toJSON css ]
        toPair (ConfStatementBlock (Block ks css)) = [ Text.pack (show ks) .= toJSON css ]
        toPair ConfStatementEmptyLine = []
        toPair (ConfStatementComment _) = []

