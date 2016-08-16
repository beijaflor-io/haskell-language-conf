{-# LANGUAGE OverloadedStrings #-}
module Data.Conf.AesonSpec where

import           Data.Aeson
import           Data.Conf.Aeson
import           Data.Conf.Types

import           Test.Hspec

spec :: Spec
spec = do
    describe "fromValue" $
        it "should convert objects to ConfStatements" $ do
             let inp = object [ "hello" .= String "world"
                              , "object" .= object [ "here" .= Number 8888
                                                   ]
                              ]
             fromJSON inp `shouldBe` Success [ ConfStatementExpression (Expression "hello" ["world"])
                                             , ConfStatementBlock (Block ["object"] [ ConfStatementExpression (Expression "here" ["8888"])
                                                                                    ])
                                             ]

    describe "toValue" $
        it "should convert objects to ConfStatements" $ do
             let inp = [ ConfStatementExpression (Expression "hello" ["world"])
                       , ConfStatementBlock (Block ["object"] [ ConfStatementExpression (Expression "here" ["8888"])
                                                              ])
                       ]
             toJSON inp `shouldBe` object [ "hello" .= String "world"
                                          , "object" .= object [ "here" .= String "8888"
                                                               ]
                                          ]
