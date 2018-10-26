{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-| A demonstration of basic functionality provided by the DocuSign Client. -}
module Main where

import Control.Monad                  ( void )
import Control.Monad.IO.Class         ( MonadIO, liftIO )
import Data.Text                      ( Text )
import Data.UUID                      ( UUID )
import DocuSign.Client                ( DocuSignClient (..)
                                      , docuSignClient
                                      , runClient )
import DocuSign.Client.Configuration  ( Config (..)
                                      , AccountConfig (..)
                                      , ServerConfig (..) )
import DocuSign.Client.Types          ( Document (..)
                                      , DocumentId
                                      , Envelope (..)
                                      , Recipient (..)
                                      , Uri )
import GHC.Generics                   ( Generic )
import Options.Generic                ( ParseField
                                      , ParseFields
                                      , ParseRecord
                                      , type (<?>) (..) )
import System.FilePath                ( takeFileName )

import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified DocuSign.Base.ContentTypes as DC
import qualified DocuSign.Client.Types      as DT
import qualified Options.Generic            as O

data CommandLineOptions = CommandLineOptions
  { host     :: Text     <?> "DocuSign server hostname"
  , port     :: Word     <?> "DocuSign server port"
  , account  :: Word     <?> "DocuSign account ID"
  , key      :: UUID     <?> "DocuSign account key"
  , username :: Text     <?> "DocuSign account username (email address)"
  , password :: Text     <?> "DocuSign account password"
  , anchor   :: Text     <?> "Anchor text to determine signature location"
  , input    :: FilePath <?> "Path to unsigned document (PDF)"
  , output   :: FilePath <?> "Path to write signed document (PDF)"
  } deriving (Generic, Show)

main :: IO ()
main = runExample =<< O.getRecord "docusign-example"

runExample :: CommandLineOptions -> IO ()
runExample options =
  runClient (makeDocuSignConfig options) (example docuSignClient options) >>=
  either
    (\e -> putStrLn "Error:" >> print e)
    (const $ pure ())

-- | A simple example of embedded document signing.
--
-- See the following guide for further details:
--
-- https://developers.docusign.com/esign-rest-api/guides/embedded-signing
--
example :: MonadIO m => DocuSignClient m -> CommandLineOptions -> m ()
example DocuSignClient {..} options@CommandLineOptions {..} = do

  liftIO $ putStrLn "Loading document ..."
  doc <- liftIO $ readInputDocument options

  liftIO $ putStrLn "Sending document to DocuSign ..."
  let aid = DT.mkAccountId $ fromIntegral $ getOption account
  (eid, url) <- postDocumentsForRedirectionBasedSigning
                  aid [doc] defaultEnvelope
                  (makeRecipient options) (const postSigningUri)

  liftIO $ do
    putStrLn "A new envelope was created with the following ID:"
    print eid
    putStrLn "Your document is now ready for signing at the link below:"
    print url
    putStrLn "After you've signed the document, press [Enter]."
    void getLine

  liftIO $ putStrLn "Fetching signed document ..."
  signedDoc <- fetchDocument aid eid defaultDocumentId

  liftIO $ do
    putStrLn "Saving signed document ..."
    B.writeFile (getOption output) (DC.toBytes signedDoc)
    putStrLn "Finished!"

defaultDocumentId :: DocumentId
defaultDocumentId = DT.mkDocumentId 1

defaultEnvelope :: Envelope
defaultEnvelope = Envelope
  { envelopeSubject = "Please sign this document"
  , envelopeMessage = "Please sign this document" }

makeDocuSignConfig :: CommandLineOptions -> Config
makeDocuSignConfig CommandLineOptions {..} =
    Config AccountConfig {..} ServerConfig {..}
  where
    accountId       = DT.mkAccountId $ fromIntegral $ getOption account
    accountKey      = getOption key
    accountUsername = getOption username
    accountPassword = getOption password
    serverHost      = getOption host
    serverPort      = fromIntegral $ getOption port

makeRecipient :: CommandLineOptions -> Recipient
makeRecipient CommandLineOptions {..} = Recipient
  { recipientClientUserId        = DT.mkUserId "TestUserId"
  , recipientEmailAddress        = DT.mkEmailAddress "test@example.com"
  , recipientName                = "Test Recipient"
  , recipientSignatureAnchorText = pure $ getOption anchor }

postSigningUri :: Uri
postSigningUri = DT.mkUri "http://httpbin.org/get"

readInputDocument :: CommandLineOptions -> IO Document
readInputDocument options = do
  let inputPath = getOption $ input options
  content <- B.readFile inputPath
  pure Document
    { documentContent = DC.fromBytes content
    , documentName    = T.pack $ takeFileName inputPath
    , documentId      = defaultDocumentId }

-- Generic option parsing instances:

instance ParseRecord CommandLineOptions

instance ParseField  Word
instance ParseFields Word
instance ParseRecord Word where parseRecord = fmap O.getOnly O.parseRecord

instance ParseField  UUID
instance ParseFields UUID
instance ParseRecord UUID where parseRecord = fmap O.getOnly O.parseRecord

getOption :: (field <?> help) -> field
getOption = O.unHelpful

