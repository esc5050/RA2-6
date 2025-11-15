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

-- 5.

detailFor :: String -> String -> String
detailFor iid msg = "item=" ++ iid ++ " :: " ++ msg

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

normalize :: String -> String
normalize = map toLower . trim

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - min n (length xs)) xs

statusText :: StatusLog -> String
statusText Sucesso = "Sucesso"
statusText (Falha msg) = "Falha: " ++ msg

formatDetailedLine :: LogEntry -> String
formatDetailedLine entry =
  show (timestamp entry)
    ++ " | " ++ show (acao entry)
    ++ " | " ++ statusText (status entry)
    ++ " | " ++ detalhes entry

extractItemId :: LogEntry -> Maybe String
extractItemId entry =
  case stripPrefix "item=" (dropWhile isSpace (detalhes entry)) of
    Nothing -> Nothing
    Just rest ->
      let ident = takeWhile (\c -> c /= ' ' && c /= '|' && c /= ':') rest
       in if null ident then Nothing else Just ident

prompt :: String -> IO String
prompt label = do
  putStr label
  hFlush stdout
  getLine

logFailure :: UTCTime -> AcaoLog -> String -> String -> IO ()
logFailure time action details err =
  appendLog $ LogEntry time action (details ++ " -> " ++ err) (Falha err)

-- 6.

printMenu :: IO ()
printMenu = do
  putStrLn "\nComandos disponíveis:"
  putStrLn "  add       - adicionar novo item"
  putStrLn "  remove    - remover unidades de um item"
  putStrLn "  update    - atualizar campos de um item"
  putStrLn "  query     - consultar item"
  putStrLn "  list      - listar inventário completo"
  putStrLn "  report    - gerar relatório baseado nos logs"
  putStrLn "  help      - mostrar este menu"
  putStrLn "  exit      - sair do sistema"

handleAdd :: Inventario -> IO Inventario
handleAdd inv = do
  iid <- prompt "ID do item: "
  name <- prompt "Nome: "
  qtyInput <- prompt "Quantidade: "
  cat <- prompt "Categoria: "
  case readMaybe qtyInput of
    Nothing -> putStrLn "Quantidade inválida." >> pure inv
    Just qty -> do
      time <- getCurrentTime
      case addItem time iid name qty cat inv of
        Left err -> do
          logFailure time Add (detailFor iid "Tentativa de adicionar") err
          putStrLn $ "Erro: " ++ err
          pure inv
        Right (inv', entry) -> do
          appendLog entry
          saveInventario inv'
          putStrLn "Item adicionado com sucesso."
          pure inv'

handleRemove :: Inventario -> IO Inventario
handleRemove inv = do
  iid <- prompt "ID do item: "
  qtyInput <- prompt "Quantidade a remover: "
  case readMaybe qtyInput of
    Nothing -> putStrLn "Quantidade inválida." >> pure inv
    Just qty -> do
      time <- getCurrentTime
      case removeItem time iid qty inv of
        Left err -> do
          logFailure time Remove (detailFor iid "Tentativa de remover") err
          putStrLn $ "Erro: " ++ err
          pure inv
        Right (inv', entry) -> do
          appendLog entry
          saveInventario inv'
          putStrLn "Remoção aplicada."
          pure inv'

handleUpdate :: Inventario -> IO Inventario
handleUpdate inv = do
  iid <- prompt "ID do item: "
  newName <- prompt "Novo nome (Enter para manter): "
  newQtyInput <- prompt "Nova quantidade (Enter para manter): "
  newCat <- prompt "Nova categoria (Enter para manter): "
  
  let qtyParsed
        | null (trim newQtyInput) = Just Nothing
        | otherwise = fmap Just (readMaybe newQtyInput)
  
  case qtyParsed of
    Nothing -> putStrLn "Quantidade inválida." >> pure inv
    Just qtyMaybe -> do
      time <- getCurrentTime
      let updateFn item = item
            { nome = if null (trim newName) then nome item else newName
            , quantidade = maybe (quantidade item) id qtyMaybe
            , categoria = if null (trim newCat) then categoria item else newCat
            }
      case updateItem time iid updateFn inv of
        Left err -> do
          logFailure time Update (detailFor iid "Tentativa de atualizar") err
          putStrLn $ "Erro: " ++ err
          pure inv
        Right (inv', entry) -> do
          appendLog entry
          saveInventario inv'
          putStrLn "Item atualizado."
          pure inv'

handleQuery :: Inventario -> IO Inventario
handleQuery inv = do
  iid <- prompt "ID do item: "
  time <- getCurrentTime
  case queryItem time iid inv of
    Left err -> do
      logFailure time QueryFail (detailFor iid "Consulta falhou") err
      putStrLn $ "Erro: " ++ err
      pure inv
    Right (item, entry) -> do
      appendLog entry
      putStrLn $ "Item encontrado: " ++ show item
      pure inv

handleList :: Inventario -> IO ()
handleList inv
  | Map.null inv = putStrLn "Inventário vazio."
  | otherwise = do
      putStrLn "\n--- Inventário Atual ---"
      mapM_ printItem (Map.elems inv)
      putStrLn "------------------------"
  where
    printItem item =
      putStrLn $ "- " ++ itemID item ++ ": " ++ nome item ++
                   " | qtd=" ++ show (quantidade item) ++
                   " | categoria=" ++ categoria item

handleReport :: Inventario -> IO Inventario
handleReport inv = do
  entries <- loadLogEntries
  if null entries
    then putStrLn "Nenhum log disponível para gerar relatório." >> pure inv
    else do
      putStrLn "\n===== Relatório de Auditoria ====="
      putStrLn $ "Total de entradas registradas: " ++ show (length entries)

      iid <- prompt "Item para histórico detalhado (Enter para pular): "
      let key = trim iid
      unless (null key) $ do
        let hist = historicoPorItem key entries
        putStrLn $ "Eventos para o item '" ++ key ++ "' (" ++ show (length hist) ++ "):"
        mapM_ (putStrLn . ("  " ++) . formatDetailedLine) hist

      let erros = logsDeErro entries
      putStrLn $ "\nFalhas registradas: " ++ show (length erros)
      mapM_ (putStrLn . ("  " ++) . formatDetailedLine) (take 5 erros)
      
      case itemMaisMovimentado entries of
        Nothing -> putStrLn "\nNenhuma movimentação registrada (Add/Remove/Update)."
        Just (iidTop, total) ->
          putStrLn $ "\nItem mais movimentado: " ++ iidTop ++ " (" ++ show total ++ " operações)"
      
      putStrLn "=================================="
      pure inv
