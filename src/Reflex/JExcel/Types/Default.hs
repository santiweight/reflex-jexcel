{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.JExcel.Types.Default where

import "data-default" Data.Default
import                Reflex.JExcel.Types.Types

instance Default JExcelAlign where
    def = AlignLeft

instance Default JExcelEvent where
    def = OnLoad

instance Default JExcelColumn where
    def = JExcelColumn
        { _jExcelColumn_title    = Nothing
        , _jExcelColumn_width    = Nothing
        , _jExcelColumn_type     = Nothing
        , _jExcelColumn_multiple = Nothing
        , _jExcelColumn_source   = Nothing
        , _jExcelColumn_url      = Nothing
        }

instance Default JExcel where
    def = JExcel
        { _jExcel_url                      = Nothing
        , _jExcel_data                     = Nothing
        , _jExcel_copyCompatibility        = Nothing
        , _jExcel_rows                     = Nothing
        , _jExcel_columns                  = Nothing
        , _jExcel_defaultColWidth          = Nothing
        , _jExcel_defaultColAlign          = Nothing
        , _jExcel_minSpareRows             = Nothing
        , _jExcel_minSpareCols             = Nothing
        , _jExcel_minDimensions            = Nothing
        , _jExcel_allowExport              = Nothing
        , _jExcel_includeHeadersOnDownload = Nothing
        , _jExcel_columnSorting            = Nothing
        , _jExcel_columnDrag               = Nothing
        , _jExcel_columnResize             = Nothing
        , _jExcel_rowResize                = Nothing
        , _jExcel_rowDrag                  = Nothing
        , _jExcel_editable                 = Nothing
        , _jExcel_allowInsertRow           = Nothing
        , _jExcel_allowManualInsertRow     = Nothing
        , _jExcel_allowInsertColumn        = Nothing
        , _jExcel_allowManualInsertColumn  = Nothing
        , _jExcel_allowDeleteRow           = Nothing
        , _jExcel_allowDeleteColumn        = Nothing
        , _jExcel_allowRenameColumn        = Nothing
        , _jExcel_allowComments            = Nothing
        , _jExcel_wordWrap                 = Nothing
        , _jExcel_csv                      = Nothing
        , _jExcel_csvFileName              = Nothing
        , _jExcel_csvHeaders               = Nothing
        , _jExcel_csvDelimiter             = Nothing
        , _jExcel_selectionCopy            = Nothing
        , _jExcel_mergeCells               = Nothing
        , _jExcel_toolbar                  = Nothing
        , _jExcel_search                   = Nothing
        , _jExcel_pagination               = Nothing
        , _jExcel_paginationOptions        = Nothing
        , _jExcel_fullscreen               = Nothing
        , _jExcel_lazyLoading              = Nothing
        , _jExcel_loadingSpin              = Nothing
        , _jExcel_tableOverflow            = Nothing
        , _jExcel_tableHeight              = Nothing
        , _jExcel_tableWidth               = Nothing
        , _jExcel_meta                     = Nothing
        , _jExcel_style                    = Nothing
        , _jExcel_parseFormulas            = Nothing
        , _jExcel_autoIncrement            = Nothing
        -- , _jExcel_updateTable -- functio= Nothing
        , _jExcel_nestedHeaders            = Nothing
        -- , _jExcel_contextMenu    --funct= Nothing
        , _jExcel_text                     = Nothing
        }
