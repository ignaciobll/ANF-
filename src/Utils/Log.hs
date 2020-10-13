{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Utils.Log where

import           Data.Aeson.Types               ( ToJSON(..) )

import           Data.Aeson                     ( Value(Object)
                                                , encode
                                                , (.=)
                                                )
import qualified Data.HashMap.Lazy             as HML
import           Data.Aeson                     ( object )

import qualified Data.ByteString.Lazy.Char8    as BL
import           Data.Time.Clock.POSIX          ( getPOSIXTime )

merge_aeson :: [Value] -> Value
merge_aeson = Object . HML.unions . map (\(Object x) -> x)

data LogType a = LogType a deriving Show

instance ToJSON a => ToJSON (LogType a) where
  toJSON (LogType lt) = object ["logType" .= toJSON lt]

newtype TimeStamp = TimeStamp { getTimeStamp :: Int} deriving newtype (Eq, Show, ToJSON)

data Log a b = Log a TimeStamp b

instance (ToJSON a, ToJSON b) => ToJSON (Log a b) where
  toJSON (Log a ts b) = merge_aeson [toJSON (LogType a), object ["timestamp" .= ts], toJSON b]


log :: (ToJSON a, ToJSON b) => a -> b -> IO (Log a b)
log a b = do
  ts <- TimeStamp . (round . (* 1000)) <$> getPOSIXTime
  let logEntry = Log a ts b
  BL.putStrLn . encode $ logEntry
  pure logEntry
