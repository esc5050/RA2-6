{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Map as Map
import Control.Monad (unless)
import Data.Char (isSpace, toLower)
import Data.List (maximumBy, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
-- Importar 'catch' para lidar com exceções de IO
import Control.Exception (catch, SomeException)

-- 1. TIPOS DE DADOS (Req 2.1)

data Item = Item
  { itemID :: String
  , nome :: String
  , quantidade :: Int
  , categoria :: String
  } deriving (Show, Read, Eq, Generic)

type Inventario = Map.Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | Query
  | QueryFail
  deriving (Show, Read, Eq, Generic)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq, Generic)

data LogEntry = LogEntry
  { timestamp :: UTCTime
  , acao :: AcaoLog
  , detalhes :: String
  , status :: StatusLog
  } deriving (Show, Read, Generic)

-- Type alias para clareza
type ResultadoOperacao = (Inventario, LogEntry)

-- Nomes de arquivos exigidos (Req 2.2, 3, 7)
inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaLogFile :: FilePath
auditoriaLogFile = "Auditoria.log"
