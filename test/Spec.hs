{-# LANGUAGE OverloadedStrings #-}

module Main where
import Test.Hspec
import Types
import Config
import Logic
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
    testTypes
    testConfig
    testLogic


-- Тесты для модуля Types
testTypes :: Spec
testTypes = describe "Types Tests" $ do
    testSymptomId
    testDoctorCreation


-- Тест создания и сравнения SymptomId
testSymptomId :: SpecWith ()
testSymptomId =
    it "SymptomId can be created and compared" $ do
        let id1 = SymptomId "test1"
            id2 = SymptomId "test2"
            id3 = SymptomId "test1"
        id1 `shouldBe` id3
        id1 `shouldNotBe` id2


-- Тест создания Doctor со всеми полями
testDoctorCreation :: SpecWith ()
testDoctorCreation =
    it "Doctor can be created with all fields" $ do
        let doc = Doctor
                { doctorName = DoctorName "Test Doctor"
                , doctorProfile = DoctorProfile "Test Profile"
                , keySymptoms = [SymptomId "s1"]
                , symptomWeights = Map.fromList [(SymptomId "s1", 10)]
                , isEmergency = True
                }
        doctorName doc `shouldBe` DoctorName "Test Doctor"
        isEmergency doc `shouldBe` True


-- Тесты для модуля Config
testConfig :: Spec
testConfig = describe "Config Tests" $ do
    testLoadDoctors
    testLoadSymptoms
    testValidateDoctorsSymptoms
    testCollectAllSymptoms


-- Тесты загрузки врачей
testLoadDoctors :: SpecWith ()
testLoadDoctors = describe "loadDoctors" $ do
    it "returns Left for non-existent file" testLoadDoctorsMissing
    it "successfully loads valid doctors.yaml" testLoadDoctorsValid


-- Тест отсутствующего файла врачей
testLoadDoctorsMissing :: IO ()
testLoadDoctorsMissing = do
    result <- loadDoctors "nonexistent_doctors.yaml"
    case result of
        Left err -> T.unpack err `shouldContain` "not found"
        Right _ -> expectationFailure "Should fail"


-- Тест загрузки валидного doctors.yaml
testLoadDoctorsValid :: IO ()
testLoadDoctorsValid = do
    result <- loadDoctors "config/doctors.yaml"
    case result of
        Left err -> expectationFailure $ T.unpack err
        Right docs -> length docs `shouldBe` 8


-- Тесты загрузки симптомов
testLoadSymptoms :: SpecWith ()
testLoadSymptoms = describe "loadSymptoms" $ do
    it "returns Left for non-existent file" testLoadSymptomsMissing
    it "successfully loads valid symptoms.yaml" testLoadSymptomsValid


-- Тест отсутствующего файла симптомов
testLoadSymptomsMissing :: IO ()
testLoadSymptomsMissing = do
    result <- loadSymptoms "nonexistent_symptoms.yaml"
    case result of
        Left err -> T.unpack err `shouldContain` "not found"
        Right _ -> expectationFailure "Should fail"


-- Тест загрузки валидного symptoms.yaml
testLoadSymptomsValid :: IO ()
testLoadSymptomsValid = do
    result <- loadSymptoms "config/symptoms.yaml"
    case result of
        Left err -> expectationFailure $ T.unpack err
        Right tree -> do
            nodeId tree `shouldBe` SymptomId "root"
            length (children tree) `shouldBe` 7


-- Тесты валидации симптомов врачей
testValidateDoctorsSymptoms :: SpecWith ()
testValidateDoctorsSymptoms =
    describe "validateDoctorsSymptoms" $ do
        it "detects unknown symptom IDs" testUnknownSymptomIds
        it "accepts valid doctors with existing symptoms" testValidDoctors


-- Тест обнаружения неизвестных ID симптомов
testUnknownSymptomIds :: IO ()
testUnknownSymptomIds = do
    let doc = Doctor
            { doctorName = DoctorName "Test"
            , doctorProfile = DoctorProfile "Test"
            , keySymptoms = [SymptomId "unknown_symptom"]
            , symptomWeights = Map.empty
            , isEmergency = False
            }
        allSyms = Map.empty
        result = validateDoctorsSymptoms [doc] allSyms
    case result of
        Left err -> T.unpack err `shouldContain` "Unknown"
        Right _ -> expectationFailure "Should fail"


-- Тест принятия валидных врачей
testValidDoctors :: IO ()
testValidDoctors = do
    result <- loadDoctors "config/doctors.yaml"
    case result of
        Left err -> expectationFailure $ T.unpack err
        Right docs -> do
            treeRes <- loadSymptoms "config/symptoms.yaml"
            case treeRes of
                Left err -> expectationFailure $ T.unpack err
                Right tree -> do
                    let allSyms = collectAllSymptoms tree
                        valRes = validateDoctorsSymptoms docs allSyms
                    case valRes of
                        Left err -> expectationFailure $ T.unpack err
                        Right validDocs -> length validDocs `shouldBe` 8


-- Тесты сбора всех симптомов
testCollectAllSymptoms :: SpecWith ()
testCollectAllSymptoms = describe "collectAllSymptoms" $
    it "collects all symptoms from tree" testCollectSymptoms


-- Тест сбора симптомов из дерева
testCollectSymptoms :: IO ()
testCollectSymptoms = do
    treeRes <- loadSymptoms "config/symptoms.yaml"
    case treeRes of
        Left err -> expectationFailure $ T.unpack err
        Right tree -> do
            let allSyms = collectAllSymptoms tree
            Map.size allSyms `shouldBe` 36
            Map.member (SymptomId "fever") allSyms `shouldBe` True


-- Тесты для модуля Logic
testLogic :: Spec
testLogic = describe "Logic Tests" $ do
    describe "chooseDoctor" $ do
        it "returns NotEnoughSymptoms for empty selection" $
            testEmptySelection
        it "returns NotEnoughSymptoms for <3 symptoms" $
            testLessThanThreeSymptoms
        it "detects emergency cases" $
            testEmergencyCase
        it "exact match works for key symptoms" $
            testExactMatch
        it "weight-based rule works with 3+ symptoms" $
            testWeightBased
        it "returns Unclear when no match" $
            testUnclear


-- Вспомогательная функция создания врача для тестов
testDoctor :: T.Text -> [T.Text] -> [(T.Text, Int)] -> Bool -> Doctor
testDoctor name keySyms weights emergency = Doctor
    { doctorName = DoctorName name
    , doctorProfile = DoctorProfile "Test"
    , keySymptoms = map SymptomId keySyms
    , symptomWeights = Map.fromList
        [(SymptomId k, v) | (k, v) <- weights]
    , isEmergency = emergency
    }


-- Тест пустого выбора симптомов
testEmptySelection :: IO ()
testEmptySelection = do
    let docs = [testDoctor "D1" ["fever"] [] False]
        selected = Set.empty
    chooseDoctor docs selected `shouldBe` NotEnoughSymptoms


-- Тест недостаточного количества симптомов
testLessThanThreeSymptoms :: IO ()
testLessThanThreeSymptoms = do
    let docs = [testDoctor "D1" ["fever"] [("headache", 5)] False]
        selected = Set.fromList [SymptomId "headache"]
    chooseDoctor docs selected `shouldBe` NotEnoughSymptoms


-- Тест экстренного случая
testEmergencyCase :: IO ()
testEmergencyCase = do
    let emergencyDoc = testDoctor "ER" ["chest_pain"] [] True
        normalDoc = testDoctor "Normal" ["chest_pain"] [] False
        docs = [normalDoc, emergencyDoc]
        selected = Set.singleton $ SymptomId "chest_pain"
    case chooseDoctor docs selected of
        DoctorsFound diags -> do
            let erDiag = head $ filter
                    (\d -> take 9 (ddRule d) == "Emergency") diags
            doctorName (ddDoctor erDiag) `shouldBe` DoctorName "ER"
        _ -> expectationFailure "Should find emergency doctor"


-- Тест точного совпадения ключевых симптомов
testExactMatch :: IO ()
testExactMatch = do
    let doc1 = testDoctor "D1" ["fever"] [] False
        doc2 = testDoctor "D2" ["headache"] [] False
        docs = [doc1, doc2]
        selected = Set.singleton $ SymptomId "headache"
    case chooseDoctor docs selected of
        DoctorsFound [diag] -> do
            take 5 (ddRule diag) `shouldBe` "Exact"
            doctorName (ddDoctor diag) `shouldBe` DoctorName "D2"
        _ -> expectationFailure "Should find exact match"


-- Тест накопительного рейтинга
testWeightBased :: IO ()
testWeightBased = do
    let doc1 = testDoctor "D1" []
            [("headache", 5), ("fever", 3)] False
        doc2 = testDoctor "D2" []
            [("headache", 2), ("fever", 1)] False
        docs = [doc1, doc2]
        selected = Set.fromList
            [SymptomId "headache", SymptomId "fever", SymptomId "other"]
    case chooseDoctor docs selected of
        DoctorsFound diags -> do
            let d1 = head $ filter (\d -> ddScore d == 8) diags
            doctorName (ddDoctor d1) `shouldBe` DoctorName "D1"
        _ -> expectationFailure "Should find by weight"


-- Тест неясного диагноза
testUnclear :: IO ()
testUnclear = do
    let doc = testDoctor "D1" ["fever"] [] False
        docs = [doc]
        selected = Set.fromList
            [SymptomId "s1", SymptomId "s2", SymptomId "s3"]
    chooseDoctor docs selected `shouldBe` Unclear
