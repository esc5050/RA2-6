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
import Control.Exception (catch, SomeException)

-- 1.

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

type ResultadoOperacao = (Inventario, LogEntry)

inventarioFile :: FilePath
inventarioFile = "Inventario.dat"

auditoriaLogFile :: FilePath
auditoriaLogFile = "Auditoria.log"

-- 2.

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem time iid name qty cat inv
  | qty <= 0 = Left "Quantidade deve ser maior que zero"
  | Map.member iid inv = Left "Item já existe no inventário (ID duplicado)"
  | otherwise = Right (newInv, logEntry)
  where
    newItem = Item iid name qty cat
    newInv = Map.insert iid newItem inv
    logEntry = LogEntry time Add (detailFor iid ("Adicionado: " ++ name)) Sucesso

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem time iid qty inv
  | qty <= 0 = Left "Quantidade a remover deve ser maior que zero"
  | otherwise =
      case Map.lookup iid inv of
        Nothing -> Left "Item não encontrado"
        Just item
          | quantidade item < qty -> Left "Estoque insuficiente"
          | otherwise -> Right (newInv item, logEntry item)
  where
    newInv item =
      let newQty = quantidade item - qty
       in if newQty == 0
            then Map.insert iid (item {quantidade = newQty}) inv
            else Map.insert iid (item {quantidade = newQty}) inv
    logEntry item =
      LogEntry time Remove (detailFor iid ("Removido: " ++ show qty ++ " unidades (restante: " ++ show (quantidade item - qty) ++ ")")) Sucesso

updateItem :: UTCTime -> String -> (Item -> Item) -> Inventario -> Either String ResultadoOperacao
updateItem time iid updateFn inv =
  case Map.lookup iid inv of
    Nothing -> Left "Item não encontrado"
    Just item ->
      let updatedItem = updateFn item
          newInv = Map.insert iid updatedItem inv
          logEntry = LogEntry time Update (detailFor iid ("Atualizado: " ++ nome updatedItem)) Sucesso
       in Right (newInv, logEntry)

queryItem :: UTCTime -> String -> Inventario -> Either String (Item, LogEntry)
queryItem time iid inv =
  case Map.lookup iid inv of
    Nothing -> Left "Item não encontrado"
    Just item -> Right (item, LogEntry time Query (detailFor iid "Consulta realizada") Sucesso)

-- 3.

loadInventario :: IO Inventario
loadInventario = (do
    exists <- doesFileExist inventarioFile
    if not exists
      then pure Map.empty
      else do
        contents <- readFile inventarioFile
        case readMaybe contents of
          Just inv -> pure inv
          Nothing -> do
            putStrLn $ "Aviso: " ++ inventarioFile ++ " corrompido. Iniciando com inventário vazio."
            pure Map.empty
  ) `catch` handleIOException Map.empty

saveInventario :: Inventario -> IO ()
saveInventario inv = writeFile inventarioFile (show inv)

appendLog :: LogEntry -> IO ()
appendLog entry = appendFile auditoriaLogFile (show entry ++ "\n")

loadLogEntries :: IO [LogEntry]
loadLogEntries = (do
    exists <- doesFileExist auditoriaLogFile
    if not exists
      then pure []
      else do
        contents <- readFile auditoriaLogFile
        let parsed = mapMaybe readMaybe (lines contents) 
        pure parsed
  ) `catch` handleIOException []

loadLogHistory :: IO ()
loadLogHistory = (do
    exists <- doesFileExist auditoriaLogFile
    if not exists
      then putStrLn "Nenhum log de auditoria anterior encontrado."
      else do
        logContent <- readFile auditoriaLogFile
        let preview = takeLast 5 (lines logContent)
        putStrLn $ "Últimos 5 registros (" ++ auditoriaLogFile ++ "):"
        mapM_ putStrLn (if null preview then ["(Arquivo vazio)"] else preview)
  ) `catch` handleIOException ()

handleIOException :: a -> SomeException -> IO a
handleIOException defaultValue e = do
    putStrLn $ "Erro de I/O ao ler arquivo: " ++ show e
    putStrLn "Iniciando com estado vazio."
    pure defaultValue
    
-- 4. 

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem iid = filter (matches iid)
  where
    matches key entry = extractItemId entry == Just key

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter isFailure
  where
    isFailure entry = case status entry of
      Falha _ -> True
      Sucesso -> False

itemMaisMovimentado :: [LogEntry] -> Maybe (String, Int)
itemMaisMovimentado entries
  | Map.null contagens = Nothing
  | otherwise = Just $ maximumBy (comparing snd) (Map.toList contagens)
  where
    relevantes = filter ((`elem` [Add, Remove, Update]) . acao) entries
    chaves = mapMaybe extractItemId relevantes
    contagens = Map.fromListWith (+) (map (\iid -> (iid, 1)) chaves)
