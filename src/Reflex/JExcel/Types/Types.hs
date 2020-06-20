{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.JExcel.Types.Types where

import "base"  GHC.Generics (Generic)
import "base"  Data.Data (Data)
import "base"  Data.Typeable (Typeable)
import "text"  Data.Text (Text)
import "aeson" Data.Aeson (Value)


-- https://bossanova.uk/jexcel/v3/docs/quick-reference

type ColumnName = Text
type Class      = Text

newtype URL
    = URL Text
    deriving (Show, Read, Eq, Data, Typeable, Generic)

newtype CSS
    = CSS Text
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data JExcelAlign
    = AlignLeft
    | AlignCenter
    | AlignRight
    deriving (Show, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

type Cell = (Int, Int)

data JExcelEvent
    = OnLoad
    | OnBeforeChange Cell Text Text
    | OnChange Cell Text Text
    | OnAfterChange
    | OnInsertRow
    | OnDeleteRow
    | OnInsertColumn
    | OnDeleteColumn
    | OnSelection Cell Cell
    | OnSort
    | OnResize
    | OnMoveRow
    | OnFocus
    | OnBlur
    deriving (Show, Eq, Data, Typeable, Generic)


data JExcelColumnType
    = JExcelColumnText
    | JExcelColumnNumeric
    | JExcelColumnCalnder
    | JExcelColumnImage
    | JExcelColumnDropdown
    | JExcelColumnColor
    | JExcelColumnCheckbox
    deriving (Show, Eq, Data, Typeable, Generic)


data JExcelColumn
    = JExcelColumn
    { _jExcelColumn_title    :: !(Maybe Text)
    , _jExcelColumn_width    :: !(Maybe Int)
    , _jExcelColumn_type     :: !(Maybe JExcelColumnType)
    , _jExcelColumn_multiple :: !(Maybe Bool)
    , _jExcelColumn_source   :: !(Maybe Value)
    , _jExcelColumn_url      :: !(Maybe URL)
    }
    deriving (Show, Eq, Data, Typeable, Generic)


data JExcel
    = JExcel
    { _jExcel_url                      :: !(Maybe URL)
    , _jExcel_data                     :: !(Maybe [[Text]])
    , _jExcel_copyCompatibility        :: !(Maybe Bool)
    , _jExcel_rows                     :: !(Maybe Value)
    , _jExcel_columns                  :: !(Maybe [JExcelColumn])
    , _jExcel_defaultColWidth          :: !(Maybe Int)
    , _jExcel_defaultColAlign          :: !(Maybe JExcelAlign)
    , _jExcel_minSpareRows             :: !(Maybe [Int])
    , _jExcel_minSpareCols             :: !(Maybe [Int])
    , _jExcel_minDimensions            :: !(Maybe (Int, Int))
    , _jExcel_allowExport              :: !(Maybe Bool)
    , _jExcel_includeHeadersOnDownload :: !(Maybe Bool)
    , _jExcel_columnSorting            :: !(Maybe Bool)
    , _jExcel_columnDrag               :: !(Maybe Bool)
    , _jExcel_columnResize             :: !(Maybe Bool)
    , _jExcel_rowResize                :: !(Maybe Bool)
    , _jExcel_rowDrag                  :: !(Maybe Bool)
    , _jExcel_editable                 :: !(Maybe Bool)
    , _jExcel_allowInsertRow           :: !(Maybe Bool)
    , _jExcel_allowManualInsertRow     :: !(Maybe Bool)
    , _jExcel_allowInsertColumn        :: !(Maybe Bool)
    , _jExcel_allowManualInsertColumn  :: !(Maybe Bool)
    , _jExcel_allowDeleteRow           :: !(Maybe Bool)
    , _jExcel_allowDeleteColumn        :: !(Maybe Bool)
    , _jExcel_allowRenameColumn        :: !(Maybe Bool)
    , _jExcel_allowComments            :: !(Maybe Bool)
    , _jExcel_wordWrap                 :: !(Maybe Bool)
    , _jExcel_csv                      :: !(Maybe URL)
    , _jExcel_csvFileName              :: !(Maybe Text)
    , _jExcel_csvHeaders               :: !(Maybe Bool)
    , _jExcel_csvDelimiter             :: !(Maybe Text)
    , _jExcel_selectionCopy            :: !(Maybe Bool)
    , _jExcel_mergeCells               :: !(Maybe Value)
    , _jExcel_toolbar                  :: !(Maybe Value)
    , _jExcel_search                   :: !(Maybe Bool)
    , _jExcel_pagination               :: !(Maybe Int)
    , _jExcel_paginationOptions        :: !(Maybe [Int])
    , _jExcel_fullscreen               :: !(Maybe Bool)
    , _jExcel_lazyLoading              :: !(Maybe Bool)
    , _jExcel_loadingSpin              :: !(Maybe Bool)
    , _jExcel_tableOverflow            :: !(Maybe Bool)
    , _jExcel_tableHeight              :: !(Maybe CSS)
    , _jExcel_tableWidth               :: !(Maybe CSS)
    , _jExcel_meta                     :: !(Maybe Value)
    , _jExcel_style                    :: !(Maybe Value)
    , _jExcel_parseFormulas            :: !(Maybe Bool)
    , _jExcel_autoIncrement            :: !(Maybe Bool)
    -- , _jExcel_updateTable -- function
    , _jExcel_nestedHeaders            :: !(Maybe Value)
    -- , _jExcel_contextMenu    --function
    , _jExcel_text                     :: !(Maybe Value)
    } deriving (Show, Eq, Data, Typeable, Generic)

