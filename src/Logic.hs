{-# LANGUAGE OverloadedStrings #-}

-- Бизнес-логика выбора врача
module Logic where

import Types
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (sortOn)

-- Выбирает подходящих врачей по выбранным симптомам
chooseDoctor :: [Doctor] -> Set.Set SymptomId -> DiagnosisResult
chooseDoctor docs selected
    | Set.null selected = NotEnoughSymptoms
    | otherwise = processDoctorSelection docs selected


-- Обработка выбора врачей с применением всех правил
processDoctorSelection :: [Doctor] -> Set.Set SymptomId -> DiagnosisResult
processDoctorSelection docs selected =
    let emergency = findEmergency docs selected
        exact = findExactMatch docs selected
        weight = findWeightBased docs selected
        allRes = emergency ++ exact ++ weight
        unique = removeDuplicates allRes
    in if null unique
        then checkNotEnoughSymptoms selected
        else DoctorsFound unique


-- Проверка, достаточно ли симптомов для диагноза
checkNotEnoughSymptoms :: Set.Set SymptomId -> DiagnosisResult
checkNotEnoughSymptoms selected =
    if Set.size selected < 3
        then NotEnoughSymptoms
        else Unclear


-- Удаление дубликатов врачей с сохранением наивысшего приоритета
removeDuplicates :: [DoctorDiagnosis] -> [DoctorDiagnosis]
removeDuplicates = nubBy (\a b -> ddDoctor a == ddDoctor b) . sortByPriority
  where
    sortByPriority = sortOn (\d -> (priority d, - (ddScore d :: Int)))
    
    priority d
        | take 9 (ddRule d) == "Emergency" = 1 :: Int
        | take 5 (ddRule d) == "Exact" = 2 :: Int
        | otherwise = 3 :: Int
    
    nubBy _ [] = []
    nubBy eq (x:xs) = x : nubBy eq (filter (not . eq x) xs)


-- Поиск врачей по правилу экстренных случаев
findEmergency :: [Doctor] -> Set.Set SymptomId -> [DoctorDiagnosis]
findEmergency ds sel =
    [ DoctorDiagnosis d "Emergency (red flag)" syms 100
    | d <- filter isEmergency ds
    , let syms = filter (`Set.member` sel) (keySymptoms d)
    , not (null syms) ]


-- Поиск врачей по правилу точного совпадения ключевых симптомов
findExactMatch :: [Doctor] -> Set.Set SymptomId -> [DoctorDiagnosis]
findExactMatch ds sel =
    [ DoctorDiagnosis d "Exact key symptom match" syms 50
    | d <- ds
    , not (isEmergency d)
    , let syms = filter (`Set.member` sel) (keySymptoms d)
    , not (null syms) ]


-- Поиск врачей по правилу накопительного рейтинга
findWeightBased :: [Doctor] -> Set.Set SymptomId -> [DoctorDiagnosis]
findWeightBased ds sel =
    [ DoctorDiagnosis d ruleText syms score
    | d <- ds
    , let syms = Set.toList $ Set.intersection sel
                 $ Map.keysSet (symptomWeights d)
    , let score = calculateWeightScore d syms
    , score > 0
    , Set.size sel >= 3
    , let ruleText = "Weight-based (score: " ++ show score ++ ")" ]


-- Подсчёт суммарного веса для выбранных симптомов
calculateWeightScore :: Doctor -> [SymptomId] -> Int
calculateWeightScore d syms =
    sum $ map (\s -> Map.findWithDefault 0 s (symptomWeights d)) syms