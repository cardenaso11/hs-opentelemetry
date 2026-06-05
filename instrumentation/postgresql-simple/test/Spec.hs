{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString.Char8 as C
import Data.IORef (IORef, readIORef)
import qualified Data.Text as T
import OpenTelemetry.Attributes (lookupAttribute, toAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.PostgresqlSimple
  ( Only (..)
  , close
  , connectPostgreSQL
  , extractOperationName
  , pgsSpan
  , query
  )
import OpenTelemetry.Trace
  ( TracerProviderOptions (..)
  , createTracerProvider
  , emptyTracerProviderOptions
  , forceFlushTracerProvider
  , shutdownTracerProvider
  )
import OpenTelemetry.Trace.Core
  ( ImmutableSpan (..)
  , SpanHot (..)
  , setGlobalTracerProvider
  )
import OpenTelemetry.Trace.Id.Generator.Default (defaultIdGenerator)
import System.Environment (lookupEnv)
import Test.Hspec


main :: IO ()
main = hspec spec


-- | Set up an in-memory tracer provider as the global provider, run the
-- action with the span-collecting 'IORef', then shut down.
withTestTracer :: (IORef [ImmutableSpan] -> IO ()) -> IO ()
withTestTracer action = do
  (processor, ref) <- inMemoryListExporter
  tp <-
    createTracerProvider [processor] $
      emptyTracerProviderOptions
        { tracerProviderOptionsIdGenerator = defaultIdGenerator
        }
  setGlobalTracerProvider tp
  action ref
  _ <- forceFlushTracerProvider tp Nothing
  _ <- shutdownTracerProvider tp Nothing
  pure ()


spec :: Spec
spec = do
  describe "extractOperationName" $ do
    it "extracts SELECT" $
      extractOperationName "SELECT * FROM users WHERE id = ?" `shouldBe` Just "SELECT"

    it "extracts INSERT" $
      extractOperationName "INSERT INTO users (name, email) VALUES (?, ?)" `shouldBe` Just "INSERT"

    it "extracts UPDATE" $
      extractOperationName "UPDATE users SET active = true WHERE id = ?" `shouldBe` Just "UPDATE"

    it "extracts DELETE" $
      extractOperationName "DELETE FROM users WHERE expired = true" `shouldBe` Just "DELETE"

    it "handles leading whitespace and newlines" $
      extractOperationName "  \n\t  SELECT 1" `shouldBe` Just "SELECT"

    it "uppercases mixed-case keywords" $
      extractOperationName "select * from t" `shouldBe` Just "SELECT"

    it "returns Nothing for bare parenthesized subquery" $
      extractOperationName "(SELECT 1)" `shouldBe` Nothing

    it "returns Nothing for empty input" $
      extractOperationName C.empty `shouldBe` Nothing

    it "returns Nothing for whitespace only" $
      extractOperationName "   \t\n  " `shouldBe` Nothing

    it "extracts CREATE" $
      extractOperationName "CREATE TABLE IF NOT EXISTS foo (id INT)" `shouldBe` Just "CREATE"

    it "extracts ALTER" $
      extractOperationName "ALTER TABLE users ADD COLUMN age INT" `shouldBe` Just "ALTER"

    it "extracts DROP" $
      extractOperationName "DROP TABLE IF EXISTS temp_data" `shouldBe` Just "DROP"

    it "extracts EXPLAIN" $
      extractOperationName "EXPLAIN ANALYZE SELECT * FROM users" `shouldBe` Just "EXPLAIN"

  -- Integration tests that verify the full instrumentation pipeline.
  -- Requires a running PostgreSQL instance.
  -- Set PGS_TEST_CONNSTRING to a libpq connection string to enable.
  -- Example: PGS_TEST_CONNSTRING='dbname=postgres'
  describe "pgsSpan integration (requires PostgreSQL)" $ do
    mConnStr <- runIO $ lookupEnv "PGS_TEST_CONNSTRING"
    case mConnStr of
      Nothing ->
        it "SKIPPED - set PGS_TEST_CONNSTRING to enable" $
          pendingWith "Set PGS_TEST_CONNSTRING environment variable"
      Just connStr -> do
        it "records the template bytestring in db.statement" $
          withTestTracer $ \ref -> do
            conn <- connectPostgreSQL (C.pack connStr)
            -- Call pgsSpan directly with a template containing ?
            -- The inner action does not execute any real SQL.
            _ <- pgsSpan conn "SELECT * FROM users WHERE id = ?" (pure ())

            spans <- readIORef ref
            length spans `shouldBe` 1

            hot <- readIORef (spanHot (head spans))
            lookupAttribute (hotAttributes hot) "db.statement"
              `shouldBe` Just (toAttribute ("SELECT * FROM users WHERE id = ?" :: T.Text))

            -- Span name should be "<OPERATION> <dbname>"
            hotName hot `shouldSatisfy` T.isPrefixOf "SELECT "

            close conn

        it "query passes the template (with ?) to db.statement, not interpolated SQL" $
          withTestTracer $ \ref -> do
            conn <- connectPostgreSQL (C.pack connStr)

            -- Execute a real parameterized query
            [Only (r :: Int)] <- query conn "SELECT ? :: int" (Only (42 :: Int))
            r `shouldBe` 42

            spans <- readIORef ref
            length spans `shouldBe` 1

            hot <- readIORef (spanHot (head spans))

            -- CRITICAL: db.statement must be "SELECT ? :: int" (template)
            -- NOT "SELECT 42 :: int" (interpolated)
            lookupAttribute (hotAttributes hot) "db.statement"
              `shouldBe` Just (toAttribute ("SELECT ? :: int" :: T.Text))

            close conn
