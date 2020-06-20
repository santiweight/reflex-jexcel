{-# LANGUAGE PackageImports #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Reflex.JExcel.FFI where

import                       Prelude hiding ((!!))
import           "base"      Data.Maybe (fromJust)
import           "base"      Control.Monad.IO.Class (liftIO)
import           "lens"      Control.Lens hiding (element, (#))
import           "aeson"     Data.Aeson (toJSON, Value(..), decodeStrict, encode)
import           "aeson"     Data.Aeson as Aeson (decodeStrict)
import           "text"      Data.Text (Text, split, unpack)
import qualified "text"      Data.Text as T (length)
import           "text"      Data.Text.Encoding (encodeUtf8)
import           "jsaddle"   Language.Javascript.JSaddle
import           "jsaddle"   Language.Javascript.JSaddle.Value
import                       GHCJS.DOM.Element (IsElement, toElement, unElement)
import                       Reflex.JExcel.Types

newtype JExcelRef = JExcelRef
                  { unJExcelRef :: JSVal
                  }


type ColumnNumber = Int
type RowNumber = Int
type CellIdent = Text


class JExcelHandlers a where
    onload :: a -> JExcelEvent -> JSM ()
    onload _ _ = return ()

    onbeforechange :: a -> JExcelEvent -> JSM ()
    onbeforechange _ _ = return ()

    onchange :: a -> JExcelEvent -> JSM ()
    onchange _ _ = return ()

    onafterchange :: a -> JExcelEvent -> JSM ()
    onafterchange _ _ = return ()

    oninsertrow :: a -> JExcelEvent -> JSM ()
    oninsertrow _ _ = return ()

    ondeleterow :: a -> JExcelEvent -> JSM ()
    ondeleterow _ _ = return ()

    oninsertcolumn :: a -> JExcelEvent -> JSM ()
    oninsertcolumn _ _ = return ()

    ondeletecolumn :: a -> JExcelEvent -> JSM ()
    ondeletecolumn _ _ = return ()

    onselection :: a -> JExcelEvent -> JSM ()
    onselection _ _ = return ()

    onsort :: a -> JExcelEvent -> JSM ()
    onsort _ _ = return ()

    onresize :: a -> JExcelEvent -> JSM ()
    onresize _ _ = return ()

    onmoverow :: a -> JExcelEvent -> JSM ()
    onmoverow _ _ = return ()

    onfocus :: a -> JExcelEvent -> JSM ()
    onfocus _ _ = return ()

    onblur :: a -> JExcelEvent -> JSM ()
    onblur _ _ = return ()


--withHandler :: (ToJSString a0, MakeObject s) => s -> a0 -> (JExcelEvent -> JSM ()) -> JSM ()
--withHandler config name callback = do
    --_ <- config ^. jss name (fun $ \_ _ evts -> do
        --case length kkkk
        --let y = head evts
        --(x :: Text) <- valToText y
        --print x
        ----case head texts of
            ----Nothing -> return ()
            ----Just text -> do
                ----print text
                ------callback event -- value
        --w <- jsg "console"
        --_ <- w ^. js1 "log" evts
        ----_ <- w ^. js1 "log" name
        --return ())
    --return ()


jsvalToCell :: JSVal -> JSM Cell
jsvalToCell val' = do
    tmp  <- val' !! 0
    id'  <- tmp ^. js "id"
    text <- valToText id'
    liftIO $ print text
    let (column : row : _) = split (== '-') text
    return (read . unpack $ column, read . unpack $ row)



newJExcel :: ( IsElement element
             , JExcelHandlers handlers
             )
          => element
          -- ^ Element
          -> JExcel
          -- ^ Configuration
          -> handlers
          -- ^ JExcel handlers
          -> JSM JExcelRef
newJExcel element
          jexcelConfig
          handlers
    = do
    let js_element' = unElement . toElement  $ element
    liftIO $ print (toJSON $ jexcelConfig)
    js_config <- toJSVal . toJSON $ jexcelConfig
    js_config ^. jss "onload" (fun $ \_ _ _ -> onload handlers $ OnLoad)
    js_config ^. jss "onbeforechange" (fun $ \_ _ [_, js_cell, js_before, js_after] -> do
        cell <- jsvalToCell js_cell
        before <- valToText js_before
        after  <- valToText js_after
        onbeforechange handlers $ OnBeforeChange cell before after)
    js_config ^.jss "onchange" (fun $ \_ _ [_, js_cell, js_after, js_before] -> do
        cell <- jsvalToCell js_cell
        before <- valToText js_before
        after  <- valToText js_after
        onchange handlers $ OnChange cell after before)
    js_config ^. jss "onafterchange" (fun $ \_ _ _ -> onafterchange handlers $ OnAfterChange)
    js_config ^. jss "oninsertrow" (fun $ \_ _ _ -> oninsertrow handlers $ OnInsertRow)
    js_config ^. jss "ondeleterow" (fun $ \_ _ _ -> ondeleterow handlers $ OnDeleteRow)
    js_config ^. jss "oninsertcolumn" (fun $ \_ _ _ -> oninsertcolumn handlers $ OnInsertColumn)
    js_config ^. jss "ondeletecolumn" (fun $ \_ _ _ -> ondeletecolumn handlers $ OnDeleteColumn)
    js_config ^. jss "onselection" (fun $ \_ _ [_, x1', y1', x2', y2', origin] -> do
        (x1 :: Int) <- floor <$> valToNumber x1'
        (y1 :: Int) <- floor <$> valToNumber y1'
        (x2 :: Int) <- floor <$> valToNumber x2'
        (y2 :: Int) <- floor <$> valToNumber y2'
        liftJSM $ print (x1, y1)
        liftJSM $ print (x2, y2)
        onselection handlers $ OnSelection (x1, y1) (x2, y2))
    js_config ^. jss "onsort" (fun $ \_ _ _ -> onsort handlers $ OnSort)
    js_config ^. jss "onresize" (fun $ \_ _ _ -> onresize handlers $ OnResize)
    js_config ^. jss "onmoverow" (fun $ \_ _ _ -> onmoverow handlers $ OnMoveRow)
    js_config ^. jss "onfocus" (fun $ \_ _ _ -> onfocus handlers $ OnFocus)
    js_config ^. jss "onblur" (fun $ \_ _ _ -> onblur handlers $ OnBlur)

    ref <- jsg2 "jexcel" js_element' js_config
    return $ JExcelRef ref


getData :: JExcelRef
        -- ^ ref
        -> Bool
        -- ^ Get only highlighted
        -> JSM JSVal
getData ref
        onlyHighlighted
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getData" onlyHighlighted
    return result


setData :: JExcelRef
        -- ^ ref
        -> Maybe [[Text]]
        -- ^ Optional: new JSON data
        -> Bool
        -- ^ Ignore Spare
        -> JSM ()
setData ref
        mNewData
        ignoreSpare
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    case mNewData of
        Nothing -> return ()
        Just newData -> do
            let js_newData  = toJSON $ newData
            _ <- jexcel ^. js2 "setData" js_newData ignoreSpare
            return ()
    return ()


deleteColumn :: JExcelRef
             -- ^ ref
             -> ColumnNumber
             -- ^ Column number
             -> JSM ()
deleteColumn ref
             columnNumber
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <-jexcel ^. js1 "deleteColumn" columnNumber
    return ()


insertRow :: JExcelRef
          -- ^ ref
          -> RowNumber
          -- ^ Row number
          -> JSM ()
insertRow ref
          rowNumber
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js1 "insertRow" rowNumber
    return ()


deleteRow :: JExcelRef
          -- ^ ref
          -> RowNumber
          -- ^ Row number
          -> JSM ()
deleteRow ref
          rowNumber
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js1 "deleteRow" rowNumber
    return ()


getHeader :: JExcelRef
          -- ^ ref
          -> ColumnNumber
          -- ^ Column number
          -> JSM JSVal
getHeader ref
          columnNumber
    =  do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getHeader" columnNumber
    return result


setHeader :: JExcelRef
          -- ^ ref
          -> ColumnNumber
          -- ^ Column number
          -> Text
          -- ^ Coliumn title
          -> JSM ()
setHeader ref
          columnNumber
          columnTitle
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "setHeader" columnNumber columnTitle
    return ()


getWidth :: JExcelRef
         -- ^ ref
         -> ColumnNumber
         -- ^ Column number
         -> JSM JSVal
getWidth ref
         columnNumber
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getWidth" columnNumber
    return result

setWidth :: JExcelRef
         -- ^ ref
         -> ColumnNumber
         -- ^ Column number
         -> Int
         -- ^ New column width
         -> JSM ()
setWidth ref
         columnNumber
         newColumnWidth
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "setWidth" columnNumber newColumnWidth
    return ()


orderBy :: JExcelRef
        -- ^ ref
        -> ColumnNumber
        -- ^ Column number
        -> Int
        -- ^ Sort type
        -> JSM ()
orderBy ref
        columnNumber
        sortType
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "orderBy" columnNumber sortType
    return ()


getValue :: JExcelRef
         -- ^ ref
         -> CellIdent
         -- ^ Cell ID
         -> JSM JSVal
getValue ref
         cellIdent
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getValue" cellIdent
    return result


setValue :: JExcelRef
         -- ^ ref
         -> CellIdent
         -- ^ Cell ID
         -> JSVal
         -- ^ Value
         -> JSM ()
setValue ref
         cellIdent
         value
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "setValue" cellIdent value
    return ()


updateSelection :: JExcelRef
                -- ^ ref
                -> CellIdent
                -- ^ Start Cell
                -> CellIdent
                -- ^ End Cell
                -> Bool
                -- ^ Ignore Events
                -> JSM ()
updateSelection ref
                startCell
                endCell
                ignoreEvents
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js3 "updateSelection" startCell endCell ignoreEvents
    return ()


download :: JExcelRef
         -> JSM ()
download ref
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js0 "download"
    return ()


getConfig :: JExcelRef
          -- ^ ref
          -> Text
          -- ^ key
          -> JSM JSVal
getConfig ref
          key
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getConfig" key
    return result


setConfig :: JExcelRef
          -- ^ ref
          -> Text
          -- ^ key
          -> JSVal
          -- ^ value
          -> JSM ()
setConfig ref
          key
          value
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "setconfig" key value
    return ()


getStyle :: JExcelRef
         -- ^ ref
         -> Maybe CellIdent
         -- ^ Cell or entire table
         -> JSM JSVal
getStyle ref
         mCellIdent
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getStyle" mCellIdent
    return result


setStyle :: JExcelRef
         -- ^ ref
         -> JSVal
         -- ^ CSS Key,Value -- Map CellIdent JSVal
         -> JSM ()
setStyle ref
         styles
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js1 "setSTyle" styles
    return ()


getComments :: JExcelRef
            -- ^ ref
            -> Maybe CellIdent
            -- ^ Cell Ident or table
            -> JSM JSVal
getComments ref
            mCellIdent
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getComments" mCellIdent
    return result


setComments :: JExcelRef
            -- ^ ref
            -> CellIdent
            -- ^ Cell Ident
            -> Text
            -- ^ Comments
            -> JSM ()
setComments ref
            cellIdent
            comment
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    _ <- jexcel ^. js2 "setComments" cellIdent comment
    return ()


getMeta :: JExcelRef
        -- ^ ref
        -> Maybe CellIdent
        -- ^ Cell Ident or Table
        -> JSM JSVal
getMeta ref
        mCellIdent
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "getMeta" mCellIdent
    return result

setMeta :: JExcelRef
        -- ^ ref
        -> JSVal
        -- ^ metas
        -> JSM JSVal
setMeta ref
        metas
    = do
    jexcel <- valToObject . unJExcelRef $ ref
    result <- jexcel ^. js1 "setMeta" metas
    return result
