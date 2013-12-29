{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Types.Types where

import Data.Data
import Data.SafeCopy
import Data.Aeson
import Control.Monad
import Control.Applicative

data Id = Id { _getId :: Integer } deriving (Show, Eq, Ord, Typeable)
$(deriveSafeCopy 0 'base ''Id)


instance Enum Id where
    toEnum i = Id $ fromIntegral i
    fromEnum (Id i) = fromIntegral i

instance ToJSON Id where
    toJSON (Id i) = Number $ fromIntegral i

instance FromJSON Id where
    parseJSON n@(Number _) = Id <$> parseJSON n
    parseJSON _          = mzero

