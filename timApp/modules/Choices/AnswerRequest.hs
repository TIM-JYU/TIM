{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables#-}
module AnswerRequest where
import Data.Aeson
import GHC.Generics
data AnswerReq markup state input = ANSReq {markup :: markup, state :: Maybe state, input :: input}
                                        deriving (Show,Generic)
instance (FromJSON m,FromJSON s,FromJSON i) => FromJSON (AnswerReq m s i)
instance (ToJSON m,ToJSON s,ToJSON i) => ToJSON (AnswerReq m s i)
