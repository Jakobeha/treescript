{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Run
  ( serve
  ) where

import Server.Decode
import Server.Encode
import TreeScript hiding (fileName)

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad
import qualified Control.Monad.Catch as E
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import qualified Data.Aeson as J
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.LSP.Control as CTRL
import qualified Language.Haskell.LSP.Core as Core
import Language.Haskell.LSP.Diagnostics
import Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Language.Haskell.LSP.Utility as U
import Language.Haskell.LSP.VFS
import System.IO.Temp
import System.Exit
import qualified System.Log.Logger as L
import qualified Yi.Rope as Yi

serve :: IO ()
serve = do
  -- putStrLn "Starting server..."
  serve' handleServerStart >>= \case
    0 -> do
      T.putStrLn "Server stopped."
      exitSuccess
    code -> do
      T.putStrLn $ "Server stopped with error or interrupt."
      exitWith . ExitFailure $ code

handleServerStart :: IO ()
handleServerStart = do
  -- putStrLn "Server started. Ctrl-C to stop"
  pure ()

-- Derived from Haskell LSP example
-- (https://github.com/alanz/haskell-lsp/blob/master/example/Main.hs)
-- and Haskell IDE Engine implementation
-- (https://github.com/alanz/haskell-ide-engine/blob/master/src/Haskell/Ide/Engine/Transport/LspStdio.hs)
-- ---------------------------------------------------------------------

serve' :: IO () -> IO Int
serve' dispatcherProc = E.handle finishErr $ do
  rin  <- atomically newTChan :: IO (TChan ReactorInput)
  let dp lf = do
        liftIO $ U.logs "main.run:dp entered"
        _rpid  <- forkIO $ reactor lf rin
        liftIO $ U.logs "main.run:dp tchan"
        dispatcherProc
        liftIO $ U.logs "main.run:dp after dispatcher"
        pure Nothing
  Core.setupLogger (Just "/tmp/treescript-server.log") [] L.DEBUG
  res <- CTRL.run (getConfig, dp) (lspHandlers rin) lspOptions (Just "/tmp/treescript-server-session.log")
  U.logs $ "****** FINISHED WITH CODE: " ++ show res
  finish
  pure res
  where finishErr (e :: E.SomeException) = do
          U.logs $ "****** FINISHED WITH ERROR: " ++ show e
          finish
          pure 1
        finish = L.removeAllHandlers

-- ---------------------------------------------------------------------

-- | Callback from haskell-lsp core to convert the generic message to the
-- specific one for hie
getConfig :: J.DidChangeConfigurationNotification -> Either T.Text Config
getConfig (J.NotificationMessage _ _ (J.DidChangeConfigurationParams p)) =
  case J.fromJSON p of
    J.Success c -> Right c
    J.Error err -> Left $ T.pack err

newtype Config =
  Config
    { maxNumberOfProblems :: Int
    } deriving (Show)

instance J.FromJSON Config where
  parseJSON = J.withObject "Config" $ \v -> do
    s <- v J..: "treescript"
    flip (J.withObject "Config.settings") s $ \o -> Config
      <$> o J..: "maxNumberOfProblems"

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorInput
  = HandlerRequest FromClientMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R a = ReaderT (Core.LspFuncs Config) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

{- -- Unlike makeResponseMessage, allows for a response for an arbitrary
-- request (without request type).
makeResponseMessage' :: J.LspIdRsp -> resp -> J.ResponseMessage resp
makeResponseMessage' rid result
  = J.ResponseMessage "2.0" rid (Just result) Nothing -}

-- ---------------------------------------------------------------------

reactorSend :: FromServerMessage -> R ()
reactorSend msg = do
  lf <- ask
  liftIO $ Core.sendFunc lf msg

-- ---------------------------------------------------------------------

publishDiagnostics :: J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> R ()
publishDiagnostics uri v diags = do
  lf <- ask
  config <- liftIO $ Core.config lf
  let maxNumProbs
        = case maxNumberOfProblems <$> config of
               Nothing -> maxBound
               Just (-1) -> maxBound
               Just x -> x
  liftIO $ (Core.publishDiagnosticsFunc lf) maxNumProbs uri v diags

-- ---------------------------------------------------------------------

nextLspReqId :: R J.LspId
nextLspReqId = do
  f <- asks Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: Core.LspFuncs Config -> TChan ReactorInput -> IO ()
reactor lf inp = flip runReaderT lf $ do
  liftIO $ U.logs "reactor:entered"
  let handlers = [ E.Handler onSomeExcept
                 ]
      onSomeExcept (e :: E.SomeException) = do
        liftIO $ U.logm $ "****** EXCEPTION ******"
        liftIO $ U.logs $ show e
  forever $ (`E.catches` handlers) $ do
    HandlerRequest inval <- liftIO $ atomically $ readTChan inp
    case inval of

      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
      RspFromClient rm -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      NotInitialized _notification -> do
        liftIO $ U.logm "****** reactor: processing Initialized Notification"
        -- Server is ready, register any specific capabilities we need

         {-
         Example:
         {
                 "method": "client/registerCapability",
                 "params": {
                         "registrations": [
                                 {
                                         "id": "79eee87c-c409-4664-8102-e03263673f6f",
                                         "method": "textDocument/willSaveWaitUntil",
                                         "registerOptions": {
                                                 "documentSelector": [
                                                         { "language": "javascript" }
                                                 ]
                                         }
                                 }
                         ]
                 }
         }
        -}
        let registration = J.Registration "treescript-server-registered" J.WorkspaceExecuteCommand Nothing
            registrations = J.RegistrationParams $ J.List [registration]
        rid <- nextLspReqId

        reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

      -- -------------------------------

      NotDidOpenTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
        let doc = notification ^. J.params . J.textDocument
            uri = doc ^. J.uri
            version = Just $ doc ^. J.version
        handleChange uri version

      -- -------------------------------

      NotDidCloseTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
        -- let uri = notification ^. J.params . J.textDocument . J.uri

      -- -------------------------------

      NotDidChangeTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidChangeTextDocument"
        let doc = notification ^. J.params . J.textDocument
            uri = doc ^. J.uri
            version = doc ^. J.version
        handleChange uri version

      -- -------------------------------

      NotDidSaveTextDocument notification -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        -- let uri = notification ^. J.params . J.textDocument . J.uri

      -- -------------------------------

      ReqRename req -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        {- let params = req ^. J.params
            uri = params ^. J.textDocument . J.uri
        mdoc <- liftIO $ Core.getVirtualFileFunc lf uri
        case mdoc of
          Just (VirtualFile _ str) -> do
            liftIO $ U.logs $ "reactor:processing RenameRequest: vf got:" ++ (show $ Yi.toString str)
            let src = Yi.toText str
                loc = decodeLoc src $ params ^. J.position
                newName = params ^. J.newName
                edit = J.WorkspaceEdit
                                Nothing -- "changes" field is deprecated
                                (Just (J.List [])) -- populate with actual changes from the rename
                rspMsg = Core.makeResponseMessage req edit
            reactorSend $ RspRename rspMsg
          Nothing -> do
            liftIO $ U.logs "reactor:processing RenameRequest: vf returned Nothing" -}

      -- -------------------------------

      ReqHover req -> do
        liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
        {- let J.TextDocumentPositionParams _doc pos = req ^. J.params

        let ht = Just $ J.Hover ms (Just range)
            ms = J.List [J.CodeString $ J.LanguageString "treescript" "TYPE INFO" ]
            range = J.Range pos pos
        reactorSend $ Core.RspHover $ Core.makeResponseMessage req ht -}

      -- -------------------------------

      ReqCodeAction req -> do
        liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
        {- let params = req ^. J.params
            doc = params ^. J.textDocument
            -- fileName = drop (length ("file://"::String)) doc
            -- J.Range from to = J._range (params :: J.CodeActionParams)
            (J.List diags) = params ^. J.context . J.diagnostics

        let
          -- makeCommand only generates commands for diagnostics whose source is us
          makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "treescript") _m _l) = [J.Command title cmd cmdparams]
            where
              title = "Apply TreeScript command:" <> head (T.lines _m)
              -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
              cmd = "rename"
              -- need 'file' and 'start_pos'
              args = J.List
                      [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                      , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                      ]
              cmdparams = Just args
          makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
        let body = J.List $ map J.CACommand $ concatMap makeCommand diags
            rsp = Core.makeResponseMessage req body
        reactorSend $ Core.RspCodeAction rsp -}

      -- -------------------------------

      ReqExecuteCommand req -> do
        liftIO $ U.logs $ "reactor:got ExecuteCommandRequest:" ++ show req
        {- let params = req ^. J.params
            margs = params ^. J.arguments

        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs

        let
          reply v = reactorSend $ Core.RspExecuteCommand $ Core.makeResponseMessage req v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
          r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (J.Object mempty)
            lid <- nextLspReqId
            -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            reactorSend $ Core.ReqApplyWorkspaceEdit $ fmServerApplyWorkspaceEditRequest lid we
          Nothing ->
            reply (J.Object mempty) -}

      -- -------------------------------

      ReqCompletion req -> do
        liftIO $ U.logs $ "reactor:got CompletionRequest:" ++ show req
        {- let params = req ^. J.params
            filePath = J.uriToFilePath $ params ^. J.textDocument ^. J.uri
            pos = params ^. J.position

        mprefix <- getPrefixAtPos doc pos

        callback <- hieResponseHelper (req ^. J.id) $ \compls -> do
          let rspMsg = Core.makeResponseMessage req
                         $ J.Completions $ J.List compls
          reactorSend rspMsg
        case mprefix of
          Nothing -> liftIO $ callback $ IdeResponseOk []
          Just prefix -> do
            let hreq = IReq (req ^. J.id) callback
                         $ Hie.getCompletions doc prefix
            makeRequest hreq -}

      ReqCompletionItemResolve req -> do
        liftIO $ U.logs $ "reactor:got CompletionItemResolveRequest:" ++ show req
        {- let origCompl = req ^. J.params
            mquery = case J.fromJSON <$> origCompl ^. J.xdata of
                       Just (J.Success q) -> Just q
                       _ -> Nothing
        callback <- hieResponseHelper (req ^. J.id) $ \docs -> do
          let rspMsg = Core.makeResponseMessage req $
                         origCompl & J.documentation .~ docs
          reactorSend rspMsg
        let hreq = GReq Nothing Nothing (Just $ req ^. J.id) callback $ runExceptT $ do
              case mquery of
                Nothing -> return Nothing
                Just query -> do
                  res <- lift $ liftToGhc $ Hoogle.infoCmd' query
                  case res of
                    Right x -> return $ Just x
                    _ -> return Nothing
        makeRequest hreq -}

      -- -------------------------------

      ReqDocumentHighlights req -> do
        liftIO $ U.logs $ "reactor:got DocumentHighlightsRequest:" ++ show req
        {- let rid = J.responseId $ req ^. J.id
            params = req ^. J.params
            doc = params ^. J.textDocument
            uri = decodeUri $ doc ^. J.uri
            loc = decodeLoc $ params ^. J.position
        refsRes <- inspectFile uri (BasicInj.refsToSymbolAt loc) cache
        case refsRes of
          Failure err -> do
            let msg = Text.pack $ summary err
                rerr = J.ResponseError J.InvalidRequest msg Nothing
            Core.makeResponseError req rerr
          Just refs -> do
            let erefs = encodeReferences refs
                rsp = Core.makeResponseMessage req erefs
            reactorSend rsp -}

        {- let params = req ^. J.params
            doc = params ^. J.textDocument ^. J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \highlights -> do
          let rspMsg = Core.makeResponseMessage req $ J.List highlights
          reactorSend rspMsg
        let hreq = IReq (req ^. J.id) callback
                 $ Hie.getReferencesInDoc doc pos
        makeRequest hreq -}

      -- -------------------------------

      ReqDefinition req -> do
        liftIO $ U.logs $ "reactor:got DefinitionRequest:" ++ show req
        {- let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \loc -> do
            let rspMsg = Core.makeResponseMessage req loc
            reactorSend rspMsg
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
                     $ fmap J.MultiLoc <$> Hie.findDef doc pos
        makeRequest hreq -}

      ReqFindReferences req -> do
        liftIO $ U.logs $ "reactor:got FindReferences:" ++ show req
        {- let params = req ^. J.params
            doc = params ^. J.textDocument ^. J.uri
            pos = params ^. J.position
        callback <- hieResponseHelper (req ^. J.id) $ \highlights -> do
          let rspMsg = Core.makeResponseMessage req $ J.List highlights
          reactorSend rspMsg
        let hreq = IReq (req ^. J.id) callback
                 $ fmap (map (J.Location doc . (^. J.range)))
                 <$> Hie.getReferencesInDoc doc pos
        makeRequest hreq -}

      -- -------------------------------

      ReqDocumentFormatting req -> do
        liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
        {- let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            tabSize = params ^. J.options . J.tabSize
        callback <- hieResponseHelper (req ^. J.id) $ \textEdit -> do
            let rspMsg = Core.makeResponseMessage req $ J.List textEdit
            reactorSend rspMsg
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
                     $ Brittany.brittanyCmd tabSize doc Nothing
        makeRequest hreq -}

      -- -------------------------------

      ReqDocumentRangeFormatting req -> do
        liftIO $ U.logs $ "reactor:got FormatRequest:" ++ show req
        {- let params = req ^. J.params
            doc = params ^. J.textDocument . J.uri
            range = params ^. J.range
            tabSize = params ^. J.options . J.tabSize
        callback <- hieResponseHelper (req ^. J.id) $ \textEdit -> do
            let rspMsg = Core.makeResponseMessage req $ J.List textEdit
            reactorSend rspMsg
        let hreq = GReq (Just doc) Nothing (Just $ req ^. J.id) callback
                     $ Brittany.brittanyCmd tabSize doc (Just range)
        makeRequest hreq -}

      -- -------------------------------

      ReqDocumentSymbols req -> do
        liftIO $ U.logs $ "reactor:got Document symbol request:" ++ show req
        {- let uri = req ^. J.params . J.textDocument . J.uri
        callback <- hieResponseHelper (req ^. J.id) $ \docSymbols -> do
            let rspMsg = Core.makeResponseMessage req $ J.List docSymbols
            reactorSend rspMsg
        let hreq = IReq (req ^. J.id) callback
                 $ Hie.getSymbols uri
        makeRequest hreq -}

      -- -------------------------------

      NotCancelRequestFromClient notif -> do
        liftIO $ U.logs $ "reactor:got CancelRequest:" ++ show notif
        {- let lid = notif ^. J.params . J.id
        liftIO $ atomically $ do
          wip <- readTVar wipTVar
          when (S.member lid wip) $ do
            modifyTVar' cancelReqTVar (S.insert lid) -}

      -- -------------------------------

      NotDidChangeConfiguration notif -> do
        liftIO $ U.logs $ "reactor:didChangeConfiguration notification:" ++ show notif
        {- -- if hlint has been turned off, flush the disgnostics
        diagsOn              <- configVal True hlintOn
        maxDiagnosticsToSend <- configVal 50 maxNumberOfProblems
        liftIO $ U.logs $ "reactor:didChangeConfiguration diagsOn:" ++ show diagsOn
        -- If hlint is off, remove the diags. But make sure they get sent, in
        -- case maxDiagnosticsToSend has changed.
        if diagsOn
          then flushDiagnosticsBySource maxDiagnosticsToSend Nothing
          else flushDiagnosticsBySource maxDiagnosticsToSend (Just "hlint") -}

      -- -------------------------------

      om -> do
        liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------

handleChange :: J.Uri -> J.TextDocumentVersion -> R ()
handleChange uri version = do
  liftIO $ U.logs "********* recompiling for file change"
  case J.uriToFilePath uri of
    Nothing -> liftIO $ U.logs "********* can't recompile - no file path from uri"
    Just filePath -> do
      outPath <- liftIO $ emptySystemTempFile "treescript-server-out.trpg"
      errs <- resultErrors <$> liftIO (runSessionResReal $ compile filePath outPath)
      let diags = partitionBySource $ map encodeError errs
      publishDiagnostics uri version diags

-- ---------------------------------------------------------------------

-- toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
-- toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------

{- -- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Uri -> Maybe Int -> R ()
sendDiagnostics fileUri version = do
  let
    diags = [J.Diagnostic
              (J.Range (J.Position 0 1) (J.Position 0 5))
              (Just J.DsWarning)  -- severity
              Nothing  -- code
              (Just "treescript-server") -- source
              "Example diagnostic message"
              (Just (J.List []))
            ]
  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  publishDiagnostics 100 fileUri version (partitionBySource diags) -}

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspOptions :: Core.Options
lspOptions = def { Core.textDocumentSync = Just syncOptions
                 , Core.completionProvider = Just (J.CompletionOptions (Just True) (Just ["<", ">"]))
                 , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["rename"]))
                 }

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin ReqRename
        , Core.hoverHandler                             = Just $ passHandler rin ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
        , Core.responseHandler                          = Just $ responseHandlerCb rin
        , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
        , Core.completionHandler                        = Just $ passHandler rin ReqCompletion
        , Core.completionResolveHandler                 = Just $ passHandler rin ReqCompletionItemResolve
        , Core.documentHighlightHandler                 = Just $ passHandler rin ReqDocumentHighlights
        , Core.documentFormattingHandler                = Just $ passHandler rin ReqDocumentFormatting
        , Core.documentRangeFormattingHandler           = Just $ passHandler rin ReqDocumentRangeFormatting
        , Core.documentSymbolHandler                    = Just $ passHandler rin ReqDocumentSymbols
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------
