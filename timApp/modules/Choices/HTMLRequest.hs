{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables#-}
module HTMLRequest where
import Data.Aeson
import GHC.Generics
data HTMLReq markup state = HTMLReq {markup :: markup, state :: Maybe state } deriving (Show,Generic)
instance (FromJSON m,FromJSON s) => FromJSON (HTMLReq m s)
instance (ToJSON m,ToJSON s) => ToJSON (HTMLReq m s)
