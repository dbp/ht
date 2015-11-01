{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where


import           Control.Arrow             (first, second)
import           Control.Exception         (SomeException (..), catch)
import           Control.Lens
import           Control.Logging
import           Data.Aeson.Lens
import           Data.List                 (sort)
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.UUID
import qualified Database.Redis            as R
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment
import           Web.Fn


data Ctxt = Ctxt { _req   :: Request
                 , _redis :: R.Connection
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site

initializer :: IO Ctxt
initializer = do r <- R.connect R.defaultConnectInfo
                 return (Ctxt defaultRequest r)

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ segment // method POST ==> handler ]
    `fallthrough` notFoundText "Page not found."

instance FromParam UUID where
  fromParam x = maybe (Left ParamUnparsable) Right (fromText x)


handler :: UUID -> Ctxt -> IO (Maybe Response)
handler uuid ctxt =
  do b <- strictRequestBody (_req ctxt)
     case b ^? key "version" . _Number of
       Nothing -> return Nothing
       Just 1 -> do let name = b ^. key "name" . _String
                        balances' :: [(Text, Int)]
                        balances' = b ^@.. key "balances" . members . _Integral
                        balances :: [(Text, Int)]
                        balances = map (first (T.intercalate ",")) $ M.toList $  M.fromListWith (+) $ map (first (sort . T.splitOn ",")) balances'
                    res <- R.runRedis (_redis ctxt) $
                       do let k = "ht:api:" <> toASCIIBytes uuid
                          R.hset k (T.encodeUtf8 name) (review _JSON balances)
                          R.hgetall k
                    case res of
                      Left err -> errorL' (T.pack $ show err)
                      Right xs -> do let vs :: [(Text, [([Text], Int)])]
                                         vs = map fix xs
                                     let result = map (\(name,_) ->  (name, calculate name vs)) vs
                                     okText (review _JSON (M.fromList [("balances" :: Text, M.fromList result)]))
  where fix (name, ys) = let keys = ys ^.. values . nth 0 . _String
                             vals = ys ^.. values . nth 1 . _Integral
                         in (T.decodeUtf8 name, zip (map (T.splitOn ",") keys) vals)
        calculate :: Text -> [(Text, [([Text], Int)])] -> Int
        calculate name vs =
          let ispent = sum $ map snd $ concat $ map snd $ filter ((== name) . fst) vs
              shouldhavespent = sum $ map (getamount name) (concat $ map snd vs)
          in ispent - shouldhavespent
        getamount :: Text -> ([Text], Int) -> Int
        getamount name (names, amount) = if name `elem` names
                                          then amount `div` length names
                                          else 0
main :: IO ()
main = withStdoutLogging $
       do port <- maybe 8000 read <$> lookupEnv "PORT"
          log' $ "Starting server on port " <> T.pack (show port) <> "..."
          app' <- app
          catch (run port app')
                (\(_ :: SomeException) ->
                   do log' "Shutting down...")
