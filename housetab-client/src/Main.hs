{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Exception         (SomeException (..), catch)
import           Control.Lens
import           Control.Logging
import           Data.Aeson.Lens
import qualified Data.Csv                  as C
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.UUID
import qualified Data.Vector               as V
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Directory          (doesFileExist)
import           System.Environment
import qualified Text.XmlHtml              as X
import           Web.Fn
import           Web.Fn.Extra.Heist

import           Ledger

data Ctxt = Ctxt { _req   :: Request
                 , _heist :: FnHeistState Ctxt
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  heistLens = heist

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

splices :: Splices (FnSplice Ctxt)
splices = tag "add-class" (attr "url-ends-with" &= attr "class") addClass
  where addClass ctxt node ends cls =
          return $
          if ends `T.isSuffixOf` (T.decodeUtf8 (rawPathInfo (_req ctxt)))
             then map (appendToAttribute "class" cls) (X.childNodes node)
             else X.childNodes node
        appendToAttribute a v n =
          X.setAttribute a
                         ((fromMaybe "" $ X.getAttribute a n) <> " " <> v)
                         n

initializer :: IO Ctxt
initializer = do h <- heistInit ["templates"] splices
                 case h of
                   Left errs -> errorL' (T.intercalate "\n" . map T.pack $ ("Heist errors:" :errs))
                   Right hs ->
                     return (Ctxt defaultRequest hs)

paramMaybe :: FromParam p =>
              Text ->
              Req ->
              (Maybe p -> a) ->
              Maybe (Req, a)
paramMaybe n req k =
  let (_,q,_) = req
      match = filter ((== T.encodeUtf8 n) . fst) q
  in case foldLefts [] $ map (fromParam . maybe "" T.decodeUtf8 . snd) match of
       Right [p] -> Just (req, k (Just p))
       _ -> Just (req, k Nothing)
  where foldLefts acc [] = Right (reverse acc)
        foldLefts _ (Left x : _) = Left x
        foldLefts acc (Right x : xs) = foldLefts (x : acc) xs

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end // method GET          ==> indexHandler
             , segment // path "add"      ==> addHandler
             , segment // path "reg" /? paramMaybe "mode"
                                     /? paramMaybe "start_date"
                                     /? paramMaybe "end_date"
                                     /? paramMaybe "filter"
                                          ==> regHandler
             , segment // path "src"      ==> srcHandler
             , segment // path "settings" ==> settingsHandler
             , path "static"              ==> staticServe "static"
             ]
    `fallthrough` notFoundText "Page not found."

instance FromParam UUID where
  fromParam x = maybe (Left ParamUnparsable) Right (fromText x)


staticServe :: Text -> Ctxt -> IO (Maybe Response)
staticServe dir ctxt = do
  let p = dir : pathInfo (_req ctxt)
      path' = T.unpack $ T.intercalate "/" p
  exists <- doesFileExist path'
  return $ if exists
              then Just $ responseFile status200 [] path' Nothing
              else Nothing


indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler ctxt = render ctxt "index"

addHandler :: UUID -> Ctxt -> IO (Maybe Response)
addHandler uuid ctxt =
  route ctxt [ method GET  ==> getHandler
             , method POST ==> postHandler
             ]
  where getHandler ctxt = render ctxt "add"
        postHandler ctxt = undefined


regHandler :: UUID ->
              Maybe Text ->
              Maybe Text ->
              Maybe Text ->
              Maybe Text ->
              Ctxt ->
              IO (Maybe Response)
regHandler uuid mode' start' end' filter' ctxt =
  do let mode = fromMaybe "reg" mode'
         start = fromMaybe "1/1" start'
         end = fromMaybe "next year" end'
         filter = fromMaybe "expenses" filter'
         date = "date:\"" <> start <> " " <> end <> "\""
     case mode of
       "reg" -> do src <- runLedgerCommand uuid Reg
                               [ filter
                               , date
                               ]
                   case C.decodeByName (TL.encodeUtf8 (TL.fromStrict src)) of
                     Left err -> log' (T.pack err) >> render ctxt "reg"
                     Right (_, vec) ->
                       renderWithSplices
                         ctxt "reg"
                         (do "mode" ## textSplice mode
                             "filter" ## textSplice filter
                             "start_date" ## textSplice start
                             "end_date" ## textSplice end
                             "entries" ## mapSplices
                                        (\RegResult{..} ->
                                           runChildrenWithText
                                             (do "date" ## regDate
                                                 "desc" ## regDesc
                                                 "account" ## regAccount
                                                 "amount" ## regAmount
                                                 "balance" ## regBalance))
                                        (V.toList vec))
       "bal" -> do src <- runLedgerCommand uuid Bal
                               [ filter
                               , date
                               ]
                   case C.decodeByName (TL.encodeUtf8 (TL.fromStrict src)) of
                     Left err -> log' (T.pack err) >> render ctxt "bal"
                     Right (_, vec) ->
                       renderWithSplices
                         ctxt "bal"
                         (do "mode" ## textSplice mode
                             "filter" ## textSplice filter
                             "start_date" ## textSplice start
                             "end_date" ## textSplice end
                             "entries" ## mapSplices
                                        (\BalResult{..} ->
                                           runChildrenWithText
                                             (do "account" ## balAccount

                                                 "balance" ## balBalance
                                                 ))
                                        (V.toList vec))

srcHandler :: UUID -> Ctxt -> IO (Maybe Response)
srcHandler uuid ctxt =
  do src <- runLedgerCommand uuid Print []
     renderWithSplices ctxt "src" (tag' "source" (\_ _ -> textSplice src))

settingsHandler :: UUID -> Ctxt -> IO (Maybe Response)
settingsHandler uuid ctxt = render ctxt "settings"

main :: IO ()
main = withStdoutLogging $
       do port <- maybe 8000 read <$> lookupEnv "PORT"
          log' $ "Starting server on port " <> T.pack (show port) <> "..."
          app' <- app
          catch (run port app')
                (\(_ :: SomeException) ->
                   do log' "Shutting down...")
