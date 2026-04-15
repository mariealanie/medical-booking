{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Типы данных медицинской экспертной системы
module Types where

import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import GHC.Generics (Generic)
import Graphics.Gloss (Picture)

-- Уникальный идентификатор симптома
newtype SymptomId = SymptomId { unSymptomId :: Text }
    deriving (Show, Eq, Ord, FromJSON, ToJSON, 
              FromJSONKey, ToJSONKey, Generic)

-- Имя врача для отображения
newtype DoctorName = DoctorName { unDoctorName :: Text }
    deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Профиль врача
newtype DoctorProfile = DoctorProfile { unDoctorProfile :: Text }
    deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Название симптома
newtype SymptomName = SymptomName { unSymptomName :: Text }
    deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Узел дерева симптомов
data SymptomNode = SymptomNode
    { nodeId   :: SymptomId
    , nodeName :: SymptomName
    , children :: [SymptomNode]
    } deriving (Show, Eq, Generic)

instance FromJSON SymptomNode
instance ToJSON SymptomNode

-- Информация о враче
data Doctor = Doctor
    { doctorName      :: DoctorName
    , doctorProfile   :: DoctorProfile
    , keySymptoms     :: [SymptomId]
    , symptomWeights  :: Map.Map SymptomId Int
    , isEmergency     :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON Doctor
instance ToJSON Doctor

-- Режимы экрана
data ScreenMode = WelcomeScreen
                | MainScreen
                | ResultScreen
                | ReasonScreen
    deriving (Show, Eq)

-- Диагноз одного врача
data DoctorDiagnosis = DoctorDiagnosis
    { ddDoctor      :: Doctor
    , ddRule        :: String
    , ddMatchedSyms :: [SymptomId]
    , ddScore       :: Int
    } deriving (Show, Eq)

-- Результат диагностики
data DiagnosisResult 
    = DoctorsFound [DoctorDiagnosis]
    | NotEnoughSymptoms
    | Unclear
    deriving (Show, Eq)

-- Состояние приложения
data GameState = GameState
    { screenMode            :: ScreenMode
    , doctors               :: [Doctor]
    , symptomTree           :: SymptomNode
    , navStack              :: [SymptomNode]
    , selectedSymptoms      :: Set.Set SymptomId
    , allSymptoms           :: Map.Map SymptomId SymptomName
    , diagnosisResult       :: Maybe DiagnosisResult
    , showDiagnosis         :: Bool
    , welcomeImage          :: Maybe Picture
    , backgroundImage       :: Maybe Picture
    , resultBackgroundImage :: Maybe Picture
    , allSelectedImage      :: Maybe Picture
    , reasonBackgroundImage :: Maybe Picture
    , scrollOffset          :: Int
    , selectedScrollOffset  :: Int
    } deriving (Show)
