{-# LANGUAGE OverloadedStrings #-}

-- Загрузка и валидация конфигурации из YAML файлов
module Config where

import Types
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (doesFileExist)
import Data.Yaml (decodeFileThrow)
import Control.Exception (catch, SomeException)
import Data.List (sort, group)

-- Загружает список врачей из YAML файла
loadDoctors :: FilePath -> IO (Either Text [Doctor])
loadDoctors path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ T.pack $ 
             "Doctors config file not found: " ++ path
        else (Right <$> decodeFileThrow path) `catch` \e ->
             return $ Left $ T.pack $ 
             "Error parsing doctors: " ++ show (e :: SomeException)

-- Проверяет, что все ID симптомов врачей существуют
validateDoctorsSymptoms :: [Doctor] -> Map.Map SymptomId SymptomName 
                        -> Either Text [Doctor]
validateDoctorsSymptoms docsToValidate allSyms =
    let missing = concatMap checkDoctor docsToValidate
        checkDoctor d = 
            let syms = keySymptoms d ++ Map.keys (symptomWeights d)
            in filter (`Map.notMember` allSyms) syms
    in if null missing
       then Right docsToValidate
       else Left $ T.pack $ 
            "Unknown symptom IDs: " ++ show (map unSymptomId missing)

-- Загружает и валидирует дерево симптомов
loadSymptoms :: FilePath -> IO (Either Text SymptomNode)
loadSymptoms path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ T.pack $ 
             "Symptoms config file not found: " ++ path
        else do
            result <- (Right <$> decodeFileThrow path) `catch` \e ->
                      return $ Left $ T.pack $ 
                      "Error parsing symptoms: " ++ show (e :: SomeException)
            case result of
                Left err -> return $ Left err
                Right tree -> return $ validateSymptomTree tree

-- Проверяет дерево на дубликаты ID и циклы
validateSymptomTree :: SymptomNode -> Either Text SymptomNode
validateSymptomTree root =
    let ids = collectAllIds root
        sorted = sort $ map unSymptomId ids
        dups = findDuplicates sorted
    in if not (null dups)
        then Left $ T.pack $ "Duplicate IDs: " ++ show dups
        else if hasCycle root
            then Left $ T.pack "Cyclic reference detected"
            else Right root
  where
    collectAllIds node = 
        nodeId node : concatMap collectAllIds (children node)
    
    findDuplicates = map head . filter (\x -> length x > 1) . group
    
    hasCycle = hasCycle' Set.empty
      where
        hasCycle' visited node =
            let cid = nodeId node
            in if Set.member cid visited
                then True
                else any (hasCycle' (Set.insert cid visited)) 
                         (children node)

-- Собирает все симптомы в словарь
collectAllSymptoms :: SymptomNode -> Map.Map SymptomId SymptomName
collectAllSymptoms node = 
    Map.insert (nodeId node) (nodeName node) $
    foldr Map.union Map.empty $ map collectAllSymptoms (children node)
