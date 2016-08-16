{-# LANGUAGE OverloadedStrings #-}
module Data.Conf.AesonSpec where

import           Data.Aeson
import           Data.Conf.Aeson
import           Data.Conf.Types

import           Test.Hspec

spec :: Spec
spec = do
    describe "fromValue" $ do
        it "should convert objects to ConfStatements" $ do
            let inp = object [ "hello" .= String "world"
                             , "object" .= object [ "here" .= Number 8888 ]
                             ]
            fromJSON inp `shouldBe`
                Success [ ConfStatementExpression (Expression "hello"
                                                              [ "world" ])
                        , ConfStatementBlock (Block [ "object" ]
                                                    [ ConfStatementExpression (Expression "here"
                                                                                          [ "8888"
                                                                                          ])
                                                    ])
                        ]

        it "should convert from list keys words" $ do
            let inp = object [ "location /" .=
                                 object [ "here" .= String "8888" ]
                             ]
            fromJSON inp `shouldBe`
                Success [ ConfStatementBlock (Block [ "location", "/" ]
                                                    [ ConfStatementExpression (Expression "here"
                                                                                          [ "8888"
                                                                                          ])
                                                    ])
                        ]

    describe "toValue" $ do
        it "should convert objects to ConfStatements" $ do
            let inp = [ ConfStatementExpression (Expression "hello" [ "world" ])
                      , ConfStatementBlock (Block [ "object" ]
                                                  [ ConfStatementExpression (Expression "here"
                                                                                        [ "8888"
                                                                                        ])
                                                  ])
                      ]
            toJSON inp `shouldBe`
                object [ "hello" .= String "world"
                       , "object" .= object [ "here" .= String "8888" ]
                       ]

        it "should convert list keys onto words" $ do
            let inp = [ ConfStatementBlock (Block [ "location", "/" ]
                                                  [ ConfStatementExpression (Expression "here"
                                                                                        [ "8888"
                                                                                        ])
                                                  ])
                      ]
            toJSON inp `shouldBe`
                object [ "location /" .= object [ "here" .= String "8888" ] ]
