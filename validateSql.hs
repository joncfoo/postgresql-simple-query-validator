#!/usr/bin/env stack
{- stack
   --resolver lts-6.2
   --install-ghc runghc
   --package bytestring
   --package postgresql-libpq
   --package transformers
-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B
import           Database.PostgreSQL.LibPQ
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitFailure, exitSuccess)
import           Control.Monad              (forM_, zipWithM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Either                (lefts)

main :: IO ()
main =
  runExceptT (getConnectionString >>= getConnection)
  >>= \case
    Left  err  -> putStrLn err >> exitFailure
    Right conn -> checkAllQueries conn

checkAllQueries :: Connection -> IO ()
checkAllQueries conn = do
  stmts <- B.split '\0' <$> B.getContents
  results <- zipWithM fn stmts [B.pack $ "stmt" ++ show x | x <- [1..]]
  let cleanResults = filter ((/=) (Left "ignore")) results
  forM_ (zip stmts cleanResults) $ \case
    (stmt, Left e) -> B.putStrLn stmt >> putStrLn e
    _ -> return ()
  finish conn
  case lefts cleanResults of
    [] -> exitSuccess
    _  -> exitFailure
  where
    fn stmt stmtName = runExceptT (runReaderT (checkQuery stmt) (conn, stmtName))

getConnectionString :: ExceptT String IO String
getConnectionString = do
  progName <- liftIO getProgName
  liftIO getArgs >>= \case
    [cs] -> return cs
    _    -> throwE ("Usage: " ++ progName ++ " <connection string, e.g. postgresql://user:pass@localhost/mydb>")

getConnection :: String -> ExceptT String IO Connection
getConnection connStr = do
  conn <- liftIO $ connectdb (B.pack connStr)
  liftIO (status conn) >>= \case
    ConnectionOk -> return conn
    _ -> throwE =<< liftIO (getError conn "Failed to establish connection")

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
          Just e  -> if "multiple commands into a prepared statement" `B.isInfixOf` e then
                         lift (throwE "ignore")
                     else
                         lift (throwE $ B.unpack e)
