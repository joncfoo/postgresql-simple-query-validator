#!/usr/bin/env stack
{- stack
   --resolver lts-6.2
   --install-ghc runghc
   --package bytestring
   --package megaparsec
   --package postgresql-libpq
   --package transformers
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.Either                (lefts)
import           Database.PostgreSQL.LibPQ
import           Control.Monad              (forM_, zipWithM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure, exitSuccess)
import           Text.Megaparsec

main :: IO ()
main =
  runExceptT (getParams >>= processParams) >>= \case
    Left err -> putStrLn err >> exitFailure
    Right (qs, conn) -> checkAllQueries conn qs

getParams :: ExceptT String IO (FilePath, String)
getParams = do
  progName <- liftIO getProgName
  liftIO getArgs >>= \case
    [fname, connstr] -> return (fname, connstr)
    _ -> throwE ("Usage: " ++ progName ++ " <file.hs> <connection string, e.g. postgresql://user:pass@localhost/mydb>")

processParams :: (FilePath, String) -> ExceptT String IO ([ByteString], Connection)
processParams (fname, connstr) = do
  queries <- liftIO $ extractSQL fname
  conn    <- liftIO $ connectdb (B.pack connstr)
  liftIO (status conn) >>= \case
    ConnectionOk -> return (queries, conn)
    _ -> throwE =<< liftIO (getError conn "Failed to establish connection")

checkAllQueries :: Connection -> [ByteString] -> IO ()
checkAllQueries conn queries = do
  results <- zipWithM fn queries [B.pack $ "stmt" ++ show x | x <- [1..]]
  forM_ (zip queries results) $ \case
    (stmt, Left e) -> B.putStrLn stmt >> putStrLn e
    _ -> return ()
  finish conn
  case lefts results of
    [] -> exitSuccess
    _  -> exitFailure
  where
    fn stmt stmtName = runExceptT (runReaderT (checkQuery stmt) (conn, stmtName))

getError :: Connection -> String -> IO String
getError conn defmsg =
  errorMessage conn >>= \case
    Just e  -> return . B.unpack $ e
    Nothing -> return defmsg

type App = ReaderT (Connection, ByteString) (ExceptT String IO)

checkQuery :: ByteString -> App ()
checkQuery stmt = do
  (conn, stmtName) <- ask
  liftIO (prepare conn stmtName stmt Nothing)
  >>= processResult
  >> do (conn, stmtName) <- ask
        liftIO (describePrepared conn stmtName)
        >>= processResult

processResult :: Maybe Result -> App ()
processResult = \case
  Nothing -> lift (throwE "server error")
  Just r  ->
    liftIO (resultStatus r) >>= \case
      CommandOk -> return ()
      _ ->
        liftIO (resultErrorMessage r) >>= \case
          Nothing -> lift (throwE "server error")
          Just e  -> lift (throwE $ B.unpack e)

extractSQL :: FilePath -> IO [ByteString]
extractSQL fname = do
  contents <- B.readFile fname
  case parse (many $ try extract) fname contents of
    Left err -> print err >> exitFailure
    Right qs -> return $ map (swapQs . B.pack) qs
  where
    -- could this be cleaner?
    sqlqq = string "[sql|" >> someTill anyChar (string "|]")
    extract = manyTill anyChar (try.lookAhead $ string "[sql|") >> sqlqq
    swapQs stmt =
      let st = B.split '?' stmt in
      let ds = [B.pack $ "$" ++ show x | x <- [1..(length st - 1)]] ++ [""] in
      B.concat $ zipWith B.append st ds
