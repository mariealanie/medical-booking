{-# LANGUAGE OverloadedStrings #-}

-- Главный модуль
module Main where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap (loadBMP)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Exit (exitFailure)
import Types
import Config
import Logic
import UI

-- Обработчик событий
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x', y')) state =
    case screenMode state of
        WelcomeScreen -> handleWelcomeEvent state
        ResultScreen -> handleResultEvent state x' y'
        ReasonScreen -> handleReasonEvent state x' y'
        MainScreen -> handleMainEvent state x' y'
handleEvent _ state = return state


-- Обработка событий на приветственном экране
handleWelcomeEvent :: GameState -> IO GameState
handleWelcomeEvent state = return $ state { screenMode = MainScreen }


-- Обработка событий на экране результатов
handleResultEvent :: GameState -> Float -> Float -> IO GameState
handleResultEvent state x' y' = do
    let x = round x' :: Int
        y = round y' :: Int
        backClicked = x >= -390 && x <= -270 && y >= 202 && y <= 237
        reasonClicked = x >= 225 && x <= 355 && y >= -222 && y <= -177
        manySelected = Set.size (selectedSymptoms state) > 25
    if backClicked
        then return $ state { screenMode = MainScreen, showDiagnosis = False }
        else if not manySelected && reasonClicked
            then return $ state { screenMode = ReasonScreen }
            else return state


-- Обработка событий на экране объяснения
handleReasonEvent :: GameState -> Float -> Float -> IO GameState
handleReasonEvent state x' y' = do
    let x = round x' :: Int
        y = round y' :: Int
        backClicked = x >= -390 && x <= -270 && y >= 202 && y <= 237
    if backClicked
        then return $ state { screenMode = ResultScreen }
        else return state


-- Обработка событий на главном экране
handleMainEvent :: GameState -> Float -> Float -> IO GameState
handleMainEvent state x' y' = do
    let x = round x' :: Int
        y = round y' :: Int
        current = head $ navStack state
        selected = selectedSymptoms state
        selectedList = Set.toList selected
        offset = scrollOffset state
        selOffset = selectedScrollOffset state
        totalItems = length (children current)
        totalSelected = length selectedList
        visibleItems = 7
        visibleSelected = 9

    if isBackClicked x y state
        then return $ state
            { navStack = tail $ navStack state
            , showDiagnosis = False }
        else if isUpClicked x y offset
            then return $ state { scrollOffset = offset - 1 }
            else if isDownClicked x y offset totalItems visibleItems
                then return $ state { scrollOffset = offset + 1 }
                else if isUpSelectedClicked x y selOffset
                    then return $ state
                        { selectedScrollOffset = selOffset - 1 }
                    else if isDownSelectedClicked x y selOffset
                                totalSelected visibleSelected
                        then return $ state
                            { selectedScrollOffset = selOffset + 1 }
                        else if isToDoctorClicked x y
                            then return $ state
                                { diagnosisResult =
                                    Just (chooseDoctor
                                        (doctors state) selected)
                                , screenMode = ResultScreen }
                            else if isResetClicked x y
                                then return $ state
                                    { navStack = [symptomTree state]
                                    , selectedSymptoms = Set.empty
                                    , diagnosisResult = Nothing
                                    , showDiagnosis = False
                                    , scrollOffset = 0
                                    , selectedScrollOffset = 0 }
                                else handleSymptomActions state x y current
                                    selected selectedList selOffset


-- Проверка клика по кнопке BACK в центре
isBackClicked :: Int -> Int -> GameState -> Bool
isBackClicked x y state =
    let panelX = 0
        panelY = 0
        panelW = 220
        panelH = 420
    in x >= panelX - panelW `div` 2 + 15 &&
       x <= panelX - panelW `div` 2 + 85 &&
       y >= panelY + panelH `div` 2 - 39 &&
       y <= panelY + panelH `div` 2 - 11 &&
       length (navStack state) > 1


-- Проверка клика по стрелке вверх в центре
isUpClicked :: Int -> Int -> Int -> Bool
isUpClicked x y offset =
    let panelX = 0
        panelY = 0
        panelW = 220
        panelH = 420
    in x >= panelX + panelW `div` 2 - 22 &&
       x <= panelX + panelW `div` 2 - 2 &&
       y >= panelY + panelH `div` 2 - 55 &&
       y <= panelY + panelH `div` 2 - 35 &&
       offset > 0


-- Проверка клика по стрелке вниз в центре
isDownClicked :: Int -> Int -> Int -> Int -> Int -> Bool
isDownClicked x y offset totalItems visibleItems =
    let panelX = 0
        panelY = 0
        panelW = 220
        panelH = 420
    in x >= panelX + panelW `div` 2 - 22 &&
       x <= panelX + panelW `div` 2 - 2 &&
       y >= panelY - panelH `div` 2 + 35 &&
       y <= panelY - panelH `div` 2 + 55 &&
       offset + visibleItems < totalItems


-- Проверка клика по стрелке вверх в правой панели
isUpSelectedClicked :: Int -> Int -> Int -> Bool
isUpSelectedClicked x y selOffset =
    let startX = 250
        startY = 181
        itemWidth = 160
    in x >= startX + itemWidth `div` 2 - 10 &&
       x <= startX + itemWidth `div` 2 + 10 &&
       y >= startY + 25 && y <= startY + 45 &&
       selOffset > 0


-- Проверка клика по стрелке вниз в правой панели
isDownSelectedClicked :: Int -> Int -> Int -> Int -> Int -> Bool
isDownSelectedClicked x y selOffset totalSelected visibleSelected =
    let startX = 250
        startY = 181
        itemWidth = 160
    in x >= startX + itemWidth `div` 2 - 10 &&
       x <= startX + itemWidth `div` 2 + 10 &&
       y >= startY - visibleSelected * 38 - 30 &&
       y <= startY - visibleSelected * 38 - 10 &&
       selOffset + visibleSelected < totalSelected


-- Проверка клика по кнопке TO DOCTOR
isToDoctorClicked :: Int -> Int -> Bool
isToDoctorClicked x y =
    let leftBtnX = -285
        leftBtnY1 = 170
        leftBtnW = 190
        leftBtnH = 105
    in x >= leftBtnX - leftBtnW `div` 2 &&
       x <= leftBtnX + leftBtnW `div` 2 &&
       y >= leftBtnY1 - leftBtnH `div` 2 &&
       y <= leftBtnY1 + leftBtnH `div` 2


-- Проверка клика по кнопке RESET
isResetClicked :: Int -> Int -> Bool
isResetClicked x y =
    let leftBtnX = -285
        leftBtnY2 = 35
        leftBtnW = 190
        leftBtnH = 105
    in x >= leftBtnX - leftBtnW `div` 2 &&
       x <= leftBtnX + leftBtnW `div` 2 &&
       y >= leftBtnY2 - leftBtnH `div` 2 &&
       y <= leftBtnY2 + leftBtnH `div` 2


-- Проверка клика по симптому
isSymptomClicked :: Int -> Int -> Int -> SymptomNode -> Bool
isSymptomClicked x y idx _ =
    let panelX = 0
        panelY = 0
        btnY = panelY + 150 - idx * 38
    in x >= panelX - 80 && x <= panelX + 80 &&
       y >= btnY - 15 && y <= btnY + 15


-- Проверка клика по крестику удаления
isRemoveClicked :: Int -> Int -> Int -> Int -> Bool
isRemoveClicked x y selOffset idx =
    let startX = 250
        startY = 181
        itemWidth = 160
        realIdx = selOffset + idx
        crossX = startX + itemWidth `div` 2 + 8
        crossY = startY - realIdx * 38 - 2
    in x >= crossX - 15 && x <= crossX + 15 &&
       y >= crossY - 15 && y <= crossY + 15


-- Обработка действий с симптомами
handleSymptomActions :: GameState -> Int -> Int -> SymptomNode
                     -> Set.Set SymptomId -> [SymptomId] -> Int
                     -> IO GameState
handleSymptomActions state x y current selected selectedList selOffset =
    case filter (\(idx, _) -> isRemoveClicked x y selOffset idx)
                (zip [0 :: Int ..] $ drop selOffset selectedList) of
        (_, sid):_ ->
            return $ state
                { selectedSymptoms = Set.delete sid selected
                , showDiagnosis = False }
        [] -> case filter (\(idx, node) -> isSymptomClicked x y idx node)
                         (zip [0 :: Int ..] $ children current) of
            [] -> return state
            (_, node):_ ->
                if null (children node)
                    then return $ state
                        { selectedSymptoms =
                            Set.insert (nodeId node) selected
                        , showDiagnosis = False }
                    else return $ state
                        { navStack = node : navStack state
                        , showDiagnosis = False }


-- Обновление состояния
updateGame :: Float -> GameState -> IO GameState
updateGame _ = return


-- Точка входа
main :: IO ()
main = do
    let doctorsFile = "config/doctors.yaml"
        symptomsFile = "config/symptoms.yaml"
    
    putStrLn $ "Loading doctors from: " ++ doctorsFile
    putStrLn $ "Loading symptoms from: " ++ symptomsFile
    
    welcomeImg <- loadBMP "assets/IMG_7779 2.bmp"
    bgImg <- loadBMP "assets/IMG_7786.bmp"
    resultImg <- loadBMP "assets/IMG_7789.bmp"
    allSelectedImg <- loadBMP "assets/IMG_7790.bmp"
    reasonImg <- loadBMP "assets/IMG_7831.bmp"
    
    edocs <- loadDoctors doctorsFile
    case edocs of
        Left err -> putStrLn (T.unpack err) >> exitFailure
        Right docs -> do
            etree <- loadSymptoms symptomsFile
            case etree of
                Left err -> putStrLn (T.unpack err) >> exitFailure
                Right tree -> do
                    let allSyms = collectAllSymptoms tree
                    case validateDoctorsSymptoms docs allSyms of
                        Left err -> putStrLn (T.unpack err) >> exitFailure
                        Right validDocs -> do
                            putStrLn "All configurations loaded successfully!"
                            putStrLn $ "Loaded " ++ show (length validDocs) 
                                      ++ " doctors"
                            putStrLn $ "Loaded " ++ show (Map.size allSyms) 
                                      ++ " symptoms"
                            putStrLn "Starting application..."
                            let initState = GameState
                                    WelcomeScreen
                                    validDocs
                                    tree
                                    [tree]
                                    Set.empty
                                    allSyms
                                    Nothing
                                    False
                                    (Just welcomeImg)
                                    (Just bgImg)
                                    (Just resultImg)
                                    (Just allSelectedImg)
                                    (Just reasonImg)
                                    0
                                    0
                                window = InWindow 
                                    "Maria Booking - Medical Expert System" 
                                    (800, 520) 
                                    (100, 100)
                            playIO window
                                (makeColor 1.0 0.9 0.95 1) 
                                30 
                                initState 
                                renderGame 
                                handleEvent 
                                updateGame
