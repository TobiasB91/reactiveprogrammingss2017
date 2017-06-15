{-# LANGUAGE DeriveGeneric #-}

module Messages (
  Message (..),
  CommonState (..),
  AColor (..),
  ASize (..),
  TimestampedMessage (..),
  currentTimeMillis,
  timestamped,
  ServerMessage (..)
) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.MVar
import Hakka.Actor (ActorRef)

data Message = Ping | Pong | Init | Update | 
  Asteroid { 
    common :: CommonState,
    size :: ASize,
    color :: AColor } deriving (Generic, Show)

data CommonState = CommonState { 
  ident :: Int,
  pos :: (Double, Double),
  velo :: (Double, Double), 
  acc :: Double, 
  omega :: Double,
  phi :: Double
} deriving (Generic, Show) 

data ASize = Tiny | Small | Medium | Big deriving (Generic, Show) 
data AColor = Brown | Gray deriving (Generic, Show) 

currentTimeMillis :: IO Integer
currentTimeMillis = (round . (1000 *)) <$> getPOSIXTime

timestamped :: Message -> IO TimestampedMessage
timestamped msg = currentTimeMillis >>= \time -> return $ TimestampedMessage time msg

data TimestampedMessage = TimestampedMessage {
  timestamp :: Integer,
  payload :: Message
} deriving (Generic,Show)

data ServerMessage = 
    Connect {
      socketOut :: (TimestampedMessage -> IO ()),
      connectionActor :: MVar (ActorRef ServerMessage)
    }
  | Disconnect
  | Msg TimestampedMessage
  | Ok

instance Show ServerMessage where
  show (Connect _ _)  = "Connect <...>"
  show Disconnect = "Disconnect"
  show (Msg x) = "Msg " ++ show x
  show Ok = "Ok"

----------------------
-- JSON En/Decoding --

jsonOptions :: Options
jsonOptions = defaultOptions {
  sumEncoding = ObjectWithSingleField,
  allNullaryToStringTag = False
}

instance ToJSON Message where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON Message where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON TimestampedMessage where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON TimestampedMessage where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CommonState where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON CommonState where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ASize where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON ASize where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AColor where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON AColor where
  parseJSON = genericParseJSON jsonOptions
