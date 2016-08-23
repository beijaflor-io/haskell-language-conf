{-|
Module: Data.Conf.PrettyPrint
Description: Pretty-printer for 'Data.Conf'
Copyright: (c) Copyright Pedro Tacla Yamada 2016
License: MIT
Maintainer: tacla.yamada@gmail.com
Stability: experimental
Portability: unknown

import Data.Char
import Debug.Trace
Pretty-printer for "Data.Conf". Declares a 'Pretty' instance for
'ConfStatement'.
-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Conf.PrettyPrint
    (
      pPrintConf
    , Pretty (..)
    , Doc (..)
    )
  where

import           Data.Char                      (isSpace)
import           Data.Conf.Types
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Text.PrettyPrint.HughesPJClass

import           Debug.Trace

-- | Pretty-prints a 'Conf' to a 'Doc'
--
-- 'pPrint' restricted to 'Conf'
--
-- @
-- print (pPrintConf c)
-- @
--
-- Because of https://github.com/haskell/pretty/issues/26, it's not easy to
-- prevent trailing spaces while generating the output. This function patches
-- it at the end so there're no trailing spaces.
--
-- See "Text.PrettyPrint"
pPrintConf :: Conf -> Doc
pPrintConf c =
    let d = pPrint c
        ds = init (unlines (map stripEnd (lines (show d))))
    in text ds
  where
    stripEnd = reverse . dropWhile isSpace . reverse

instance Pretty ConfStatement where
    pPrint s = case s of
        ConfStatementEmptyLine -> text ""
        ConfStatementComment (Comment c) ->
            "#" <> ttext c
        ConfStatementBlock (Block ks ss) ->
            thsep ks <+> "{"
               $+$ nest 2 (pPrintList (PrettyLevel 0) ss) $+$
            "}"
        ConfStatementExpression (Expression t ts) ->
            ttext t <+> thsep ts <> ";"
    pPrintList _ ss = foldl ($+$) empty (map pPrint ss)

thsep :: [Text] -> Doc
thsep = hsep . map ttext

ttext :: Text -> Doc
ttext = text . Text.unpack
