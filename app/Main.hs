{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import                                  Prelude hiding (head)
import           "lens"                 Control.Lens
import           "text"                 Data.Text (pack)
import           "reflex-dom"           Reflex.Dom
import           "reflex-utils"         Reflex.Utils
import           "reflex-jexcel"        Reflex.JExcel

--
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()

--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://bossanova.uk/jsuites/v2/jsuites.js"
                     , css    "https://bossanova.uk/jsuites/v2/jsuites.css"
                     ]
    whenLoaded s1Ds blank $ do
        s2Ds <- sequence [ script "https://bossanova.uk/jexcel/v3/jexcel.js"
                         , css    "https://bossanova.uk/jexcel/v3/jexcel.css"
                         ]
        whenLoaded s2Ds blank blank
        return ()

--
body :: forall t m. MonadWidget t m => m ()
body = do
    -- counter
    bE <- button "next"
    counterD <- count bE
    display counterD

    -- JExcel configuration (Dynamic)
    let jexcelD = buildJExcel <$> counterD

    -- jExcel
    jexcelOutput <- jexcel (JExcelInput htmlId jexcelD)

    -- transform input to output
    let xE' = _jexcelOutput_event jexcelOutput
    let xE = ffilter isSelection xE'
    xD <- holdDyn (OnLoad) xE
    display xD

    return ()

    where
        isSelection :: JExcelEvent -> Bool
        isSelection (OnSelection _ _ ) = True
        isSelection _                  = False

        htmlId  = "excel1"

        defaultJExcel :: JExcel
        defaultJExcel
            = def
            & jExcel_columns ?~ [ def & jExcelColumn_title ?~ "First Name"
                                      & jExcelColumn_width ?~ 300
                                , def & jExcelColumn_title ?~ "Last Name"
                                      & jExcelColumn_width ?~ 80
                                , def & jExcelColumn_title ?~ "Premium"
                                      & jExcelColumn_width ?~ 100
                                , def & jExcelColumn_title ?~ "Zipcode"
                                      & jExcelColumn_width ?~ 100
                                ]

        buildJExcel :: Int -> JExcel
        buildJExcel n = defaultJExcel
                      & jExcel_data ?~ [ ["John" , "Doe"    , pack . show $ n  , "90210"]
                                       , ["Jane" , "Doe"    , "$2000"          , "10010"]
                                       , ["Johan", "Though" , "$3000"          , "20020"]
                                       ]
