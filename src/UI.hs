{-# LANGUAGE OverloadedStrings #-}

-- Отрисовка всех экранов
module UI where

import Types
import Utils
import Graphics.Gloss
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Главный диспетчер рендеринга
renderGame :: GameState -> IO Picture
renderGame state = return $ pictures $
    case screenMode state of
        WelcomeScreen -> renderWelcomeScreen state
        MainScreen    -> renderMainScreen state
        ResultScreen  -> renderResultScreen state
        ReasonScreen  -> renderReasonScreen state


-- Приветственный экран
renderWelcomeScreen :: GameState -> [Picture]
renderWelcomeScreen state = 
    case welcomeImage state of
        Just pic -> [ translate 0 0 $ scale 0.485 0.485 pic ]
        Nothing  -> renderDefaultWelcome


-- Фон приветственного экрана по умолчанию
renderDefaultWelcome :: [Picture]
renderDefaultWelcome =
    [ color (makeColor 1.0 0.85 0.9 1) $ rectangleSolid 800 520
    , translate 0 0 $ scale 0.3 0.3 $ color black $ text "CLICK" ]


-- Главный экран
renderMainScreen :: GameState -> [Picture]
renderMainScreen state = 
    renderMainBackground state
    ++ renderLeftPanel
    ++ renderCenterPanel state
    ++ renderRightPanel state
    ++ renderColumnTitles


-- Фон главного экрана
renderMainBackground :: GameState -> [Picture]
renderMainBackground state =
    case backgroundImage state of
        Just bg -> [ translate (-3) 0 $ scale 0.485 0.485 bg ]
        Nothing -> 
            [ color (makeColor 1.0 0.9 0.95 1) 
              $ rectangleSolid 800 520 ]


-- Заголовки столбцов
renderColumnTitles :: [Picture]
renderColumnTitles =
    [ translate (-77) 229 $ scale 0.14 0.14 
      $ color black $ text "Select Symptoms"
    , translate 183 229 $ scale 0.14 0.14 
      $ color black $ text "Selected Symptoms"
    ]


-- Экран с результатом
renderResultScreen :: GameState -> [Picture]
renderResultScreen state =
    let selCount = Set.size (selectedSymptoms state)
        many = selCount > 25
        bgPic = if many then allSelectedImage state 
                       else resultBackgroundImage state
        backColor = if many then makeColor 1.0 0.7 0.8 1
                           else makeColor 0.98 0.95 0.9 1
        backBtn = renderBackButton backColor
        reasonBtn = if many then [] else renderReasonButton
        mainText = if not many then renderResultText state else []
    in case bgPic of
        Just pic -> [ translate 10 0 $ scale 0.485 0.485 pic ] 
                    ++ backBtn ++ reasonBtn ++ mainText
        Nothing -> renderDefaultResult backBtn reasonBtn mainText


-- Кнопка BACK
renderBackButton :: Color -> [Picture]
renderBackButton btnColor =
    [ translate (-350) 220 
      $ color btnColor $ roundedRect 100 45 12
    , translate (-380) 210 $ scale 0.14 0.14 
      $ color black $ text "< BACK" ]


-- Кнопка REASON
renderReasonButton :: [Picture]
renderReasonButton =
    [ translate 290 (-200) 
      $ color (makeColor 0.98 0.95 0.9 1) 
      $ roundedRect 130 45 12
    , translate 245 (-213) $ scale 0.15 0.15 
      $ color black $ text "REASON" ]


-- Фон результата по умолчанию
renderDefaultResult :: [Picture] -> [Picture] -> [Picture] -> [Picture]
renderDefaultResult backBtn reasonBtn mainText =
    [ color (makeColor 1.0 0.9 0.95 1) 
      $ rectangleSolid 800 520 ] 
    ++ backBtn ++ reasonBtn ++ mainText


-- Текст результата диагностики
renderResultText :: GameState -> [Picture]
renderResultText state =
    case diagnosisResult state of
        Just (DoctorsFound infos) -> renderDoctorFound infos
        Just NotEnoughSymptoms -> renderNotEnoughSymptoms
        _ -> []


-- Отображение найденного врача
renderDoctorFound :: [DoctorDiagnosis] -> [Picture]
renderDoctorFound infos =
    let top = head infos
        name = T.unpack $ unDoctorName 
               $ doctorName $ ddDoctor top
    in [ translate (-118) (-119) $ scale 0.18 0.18 
         $ color black $ text "You need to go to:"
       , translate (-98) (-164) $ scale 0.22 0.22 
         $ color (makeColor 0.8 0.2 0.4 1) $ text name ]


-- Сообщение о недостатке симптомов
renderNotEnoughSymptoms :: [Picture]
renderNotEnoughSymptoms =
    [ translate (-118) (-119) $ scale 0.16 0.16 
      $ color (makeColor 1.0 0.5 0.0 1) 
      $ text "Not enough symptoms"
    , translate (-118) (-149) $ scale 0.12 0.12 
      $ color black $ text "Select at least 3 symptoms"
    , translate (-118) (-163) $ scale 0.12 0.12
      $ color black $ text "or a key symptom for exact match" ]


-- Экран с объяснением
renderReasonScreen :: GameState -> [Picture]
renderReasonScreen state =
    let bgPic = reasonBackgroundImage state
        backBtn = renderBackButton (makeColor 0.98 0.95 0.9 1)
        infoBox = renderInfoBox
        infoText = renderReasonText state
    in case bgPic of
        Just pic -> [ translate 0 0 $ scale 0.485 0.485 pic ] 
                    ++ backBtn ++ infoBox ++ infoText
        Nothing -> renderDefaultReason backBtn infoBox infoText


-- Прямоугольник с информацией
renderInfoBox :: [Picture]
renderInfoBox =
    [ translate 0 20 
      $ color (makeColor 0.98 0.95 0.9 1) 
      $ roundedRect 390 340 25 ]


-- Фон объяснения по умолчанию
renderDefaultReason :: [Picture] -> [Picture] -> [Picture] -> [Picture]
renderDefaultReason backBtn infoBox infoText =
    [ color (makeColor 1.0 0.9 0.95 1) 
      $ rectangleSolid 800 520 ] 
    ++ backBtn ++ infoBox ++ infoText


-- Текст объяснения
renderReasonText :: GameState -> [Picture]
renderReasonText state =
    case diagnosisResult state of
        Just (DoctorsFound infos) -> renderFullReason state infos
        Just NotEnoughSymptoms -> renderReasonNotEnough
        Just Unclear -> renderReasonUnclear
        Nothing -> []


-- Полное объяснение с топ-3 и списком остальных
renderFullReason :: GameState -> [DoctorDiagnosis] -> [Picture]
renderFullReason state infos =
    let (top3, rest) = splitAt 3 infos
        top3Text = concatMap 
            (renderTopDoctor state) (zip [0..] top3)
        restText = if null rest then [] else renderAlsoList rest
    in top3Text ++ restText


-- Объяснение при недостатке симптомов
renderReasonNotEnough :: [Picture]
renderReasonNotEnough =
    [ translate (-170) 90 $ scale 0.17 0.17 
      $ color black $ text "Not enough symptoms"
    , translate (-170) 65 $ scale 0.13 0.13 
      $ color black $ text "Select at least 3 symptoms"
    , translate (-170) 50 $ scale 0.13 0.13
      $ color black $ text "or a key symptom for exact match" ]


-- Объяснение при неясном диагнозе
renderReasonUnclear :: [Picture]
renderReasonUnclear =
    [ translate (-170) 90 $ scale 0.17 0.17 
      $ color black $ text "Unclear diagnosis"
    , translate (-170) 65 $ scale 0.13 0.13 
      $ color black $ text "No matching doctor found" ]


-- Информация о враче
renderTopDoctor :: GameState -> (Int, DoctorDiagnosis) -> [Picture]
renderTopDoctor state (idx, info) =
    let yOff :: Float
        yOff = 140 - fromIntegral idx * 75
        ruleText = "Rule: " ++ ddRule info
        symText = formatSymptomText state info
        docText = T.unpack $ unDoctorName $ doctorName $ ddDoctor info
    in [ translate (-170) yOff $ scale 0.17 0.17 
         $ color black $ text (show (idx + 1) ++ ". " ++ docText)
       , translate (-170) (yOff - 20) 
         $ scale 0.13 0.13 
         $ color (makeColor 0.3 0.3 0.4 1) $ text ruleText
       , translate (-170) (yOff - 35) 
         $ scale 0.12 0.12 
         $ color (makeColor 0.5 0.2 0.3 1) $ text symText ]


-- Форматирование текста симптомов для врача
formatSymptomText :: GameState -> DoctorDiagnosis -> String
formatSymptomText state info =
    if take 7 (ddRule info) == "Weight-"
        then "All doctor symptoms"
        else "Symptoms: " ++ formatSyms state (take 5 $ ddMatchedSyms info)


-- Форматирует список симптомов
formatSyms :: GameState -> [SymptomId] -> String
formatSyms state sids =
    intercalate ", " 
    $ map (\sid -> T.unpack $ unSymptomName 
         $ Map.findWithDefault (SymptomName "???") sid 
         $ allSymptoms state) sids


-- Список "Also:"
renderAlsoList :: [DoctorDiagnosis] -> [Picture]
renderAlsoList docs =
    let names = map (T.unpack . unDoctorName 
                    . doctorName . ddDoctor) docs
        pairs = groupIntoPairs names
        startY :: Float
        startY = -90
        lineH :: Float
        lineH = 18
    in concatMap (\(idx, lineStr) -> 
        [ translate (-170) (startY - fromIntegral idx * lineH)
          $ scale 0.12 0.12 $ color black
          $ text (if idx == (0 :: Int) then "Also: " ++ lineStr 
                                       else "      " ++ lineStr)
        ]) (zip [0..] pairs)


-- Группирует строки
groupIntoPairs :: [String] -> [String]
groupIntoPairs [] = []
groupIntoPairs [x] = [x]
groupIntoPairs (x:y:xs) = (x ++ ", " ++ y) : groupIntoPairs xs


-- Вставляет разделитель
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs


-- Левая панель
renderLeftPanel :: [Picture]
renderLeftPanel =
    let btnW = 190.0; btnH = 105.0; btnX = -285.0
        btnY1 = 170.0; btnY2 = 35.0
        btnColor = makeColor 0.85 0.75 0.65 0.95
    in renderToDoctorButton btnX btnY1 btnW btnH btnColor
       ++ renderResetButton btnX btnY2 btnW btnH btnColor


-- Кнопка TO DOCTOR
renderToDoctorButton :: Float -> Float -> Float -> Float -> Color -> [Picture]
renderToDoctorButton btnX btnY btnW btnH btnColor =
    let toDocText = "TO DOCTOR"
        toDocW :: Float
        toDocW = fromIntegral (length toDocText) * 10
    in [ translate btnX btnY $ color btnColor 
         $ roundedRect btnW btnH 15
       , translate (btnX - toDocW/2 - 28) (btnY - 16) 
         $ scale 0.20 0.20 $ color black $ text toDocText ]


-- Кнопка RESET
renderResetButton :: Float -> Float -> Float -> Float -> Color -> [Picture]
renderResetButton btnX btnY btnW btnH btnColor =
    let resetText = "RESET"
        resetW :: Float
        resetW = fromIntegral (length resetText) * 10
    in [ translate btnX btnY $ color btnColor 
         $ roundedRect btnW btnH 15
       , translate (btnX - resetW/2 - 19) (btnY - 16) 
         $ scale 0.20 0.20 $ color black $ text resetText ]


-- Центральная панель
renderCenterPanel :: GameState -> [Picture]
renderCenterPanel state =
    let cur = head $ navStack state
        sel = selectedSymptoms state
        off = scrollOffset state
        vis = 7
        allCh = children cur
        total = length allCh
        pX = 0.0; pY = 0.0; pW = 220.0; pH = 420.0
        backBtn = renderCenterBackButton state pX pY pW pH
        upArr = renderCenterUpArrow off pX pY pW pH
        downArr = renderCenterDownArrow off vis total pX pY pW pH
        visCh = take vis $ drop off allCh
        btns = concatMap (renderSymBtn sel pX pY) (zip [0..] visCh)
    in [ translate pX pY 
         $ color (makeColor 0.85 0.75 0.65 0.95) 
         $ roundedRect pW pH 20
       ] ++ backBtn ++ upArr ++ downArr ++ btns


-- Кнопка BACK в центре
renderCenterBackButton :: GameState -> Float -> Float -> Float -> Float
                       -> [Picture]
renderCenterBackButton state pX pY pW pH =
    if length (navStack state) > 1
        then [ translate (pX - pW/2 + 50) (pY + pH/2 - 25) 
               $ color (makeColor 0.6 0.4 0.3 1) 
               $ roundedRect 70 28 10
             , translate (pX - pW/2 + 15) (pY + pH/2 - 37) 
               $ scale 0.14 0.14 $ color white $ text "BACK" ]
        else []


-- Стрелка вверх в центре
renderCenterUpArrow :: Int -> Float -> Float -> Float -> Float -> [Picture]
renderCenterUpArrow off pX pY pW pH =
    if off > 0
        then [ translate (pX + pW/2 - 12) (pY + pH/2 - 45) 
               $ color black 
               $ polygon [(-10,-10), (10,-10), (0,10)] ]
        else []


-- Стрелка вниз в центре
renderCenterDownArrow :: Int -> Int -> Int -> Float -> Float -> Float -> Float
                      -> [Picture]
renderCenterDownArrow off vis total pX pY pW pH =
    if off + vis < total
        then [ translate (pX + pW/2 - 12) (pY - pH/2 + 45) 
               $ color black 
               $ polygon [(-10,10), (10,10), (0,-10)] ]
        else []


-- Кнопка симптома
renderSymBtn :: Set.Set SymptomId -> Float -> Float 
             -> (Int, SymptomNode) -> [Picture]
renderSymBtn sel pX pY (idx, node) =
    let x = pX; y :: Float; y = pY + 150 - fromIntegral idx * 38
        isSel = Set.member (nodeId node) sel
        btnCol = if isSel then makeColor 0.9 0.8 0.7 0.9 
                          else makeColor 0.98 0.95 0.9 0.9
        name = T.unpack $ unSymptomName $ nodeName node
        wc = length (words name)
        scaleF = if wc >= 2 then 0.10 else 0.13
        tX = x - 70
    in [ translate x y $ color btnCol $ roundedRect 160 30 8
       , translate tX (y - 11) $ scale scaleF scaleF 
         $ color black $ text name ]


-- Правая панель
renderRightPanel :: GameState -> [Picture]
renderRightPanel state =
    let sel = selectedSymptoms state
        selList = Set.toList sel
        symMap = allSymptoms state
        off = selectedScrollOffset state
        vis = 9
        total = length selList
        sX = 250.0; sY = 181.0; iW = 160.0
        upArr = renderRightUpArrow off sX sY iW
        downArr = renderRightDownArrow off vis total sX sY iW
        visItems = take vis $ drop off selList
        items = concatMap (renderSelItem symMap sX sY iW) 
                (zip [0..] visItems)
    in upArr ++ downArr ++ items


-- Стрелка вверх в правой панели
renderRightUpArrow :: Int -> Float -> Float -> Float -> [Picture]
renderRightUpArrow off sX sY iW =
    if off > 0
        then [ translate (sX + iW/2) (sY + 25) 
               $ color black 
               $ polygon [(-10,-10), (10,-10), (0,10)] ]
        else []


-- Стрелка вниз в правой панели
renderRightDownArrow :: Int -> Int -> Int -> Float -> Float -> Float
                     -> [Picture]
renderRightDownArrow off vis total sX sY iW =
    if off + vis < total
        then [ translate (sX + iW/2) 
               (sY - fromIntegral vis * 38 - 20) 
               $ color black 
               $ polygon [(-10,10), (10,10), (0,-10)] ]
        else []


-- Элемент выбранного симптома
renderSelItem :: Map.Map SymptomId SymptomName 
              -> Float -> Float -> Float 
              -> (Int, SymptomId) -> [Picture]
renderSelItem symMap sX sY iW (idx, sid) =
    let x = sX; y :: Float; y = sY - fromIntegral idx * 38
        name = T.unpack $ unSymptomName 
               $ Map.findWithDefault (SymptomName "???") sid symMap
        wc = length (words name)
        tX = x - iW/2 + 10
        cX = x + iW/2 + 8; cY = y - 2
        scaleF = if wc >= 2 then 0.10 else 0.12
    in [ translate x y $ color (makeColor 1.0 1.0 1.0 0.9) 
         $ roundedRect iW 30 8
       , translate tX (y - 11) 
         $ scale scaleF scaleF $ color black $ text name
       , translate cX cY 
         $ color (makeColor 0.9 0.3 0.4 1) $ circleSolid 11
       , translate (cX - 4) (cY - 10) 
         $ scale 0.12 0.12 $ color white $ text "X" ]
