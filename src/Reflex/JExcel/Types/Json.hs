{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JExcel.Types.Json where

import "base"         Data.Char (toLower)
import "aeson"        Data.Aeson.TH
import                Reflex.JExcel.Types.Types

$(deriveJSON defaultOptions ''URL)
$(deriveJSON defaultOptions ''CSS)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower . drop (length "JExcelColumn")} ''JExcelColumnType)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower . drop (length "Align")} ''JExcelAlign)
$(deriveJSON defaultOptions{constructorTagModifier = map toLower} ''JExcelEvent)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jexcelColumn_")} ''JExcelColumn)
$(deriveJSON defaultOptions{omitNothingFields = True, fieldLabelModifier = drop (length "_jexcel_")} ''JExcel)
