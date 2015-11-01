{-# LANGUAGE OverloadedStrings #-}
module Ledger where

import           Data.Csv
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Time.Clock
import           Data.UUID
import           System.Process

data Command = Reg | Accounts | Bal | Print

data RegResult = RegResult { regDate    :: Text
                           , regDesc    :: Text
                           , regAccount :: Text
                           , regAmount  :: Text
                           , regBalance :: Text
                           }

instance FromNamedRecord RegResult where
    parseNamedRecord m = RegResult <$>
                         m .: "date" <*>
                         m .: "description" <*>
                         m .: "account" <*>
                         m .: "amount" <*>
                         m .: "running total or balance"

data BalResult = BalResult { balAccount :: Text
                           , balBalance :: Text
                           }

instance FromNamedRecord BalResult where
    parseNamedRecord m = BalResult <$>
                         m .: "account" <*>
                         m .: "balance"



getLedger :: UUID -> IO Text
getLedger = undefined

setLedger :: UUID -> IO Text
setLedger = undefined

getLedgerVersions :: UUID -> IO [(UTCTime, Text)]
getLedgerVersions = undefined

runLedgerCommand :: UUID -> Command -> [Text] -> IO Text
runLedgerCommand uuid command options =
  case command of
    Print -> T.pack <$> readCreateProcess (shell ("hledger print " <> T.unpack (T.intercalate " " options))) ""
    Reg -> T.pack <$> readCreateProcess (shell ("hledger reg --output-format csv " <> T.unpack (T.intercalate " " options))) ""
    Bal -> T.pack <$> readCreateProcess (shell ("hledger bal --output-format csv " <> T.unpack (T.intercalate " " options))) ""
