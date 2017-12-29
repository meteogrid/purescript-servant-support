{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude
import Data.Argonaut.Core (Json, toString, stringify)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep, genericEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep, genericDecodeJson)
import Data.Maybe (Maybe(Just))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Global (encodeURIComponent)


-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_
  (forall a rep. Generic a rep => EncodeRep rep => a -> Json)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_
  (forall a rep. Generic a rep => DecodeRep rep => Json -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_
  (forall a rep. Generic a rep => EncodeRep rep => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_
  (forall a rep. Generic a rep => EncodeRep rep => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a rep. Generic a rep => EncodeRep rep => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a rep. Generic a rep => EncodeRep rep => a -> URLPiece
gDefaultEncodeHeader v = case genericEncodeJson v of
    x | Just s <- toString x -> s -- Special case string - just use it as is (http-api-data compatibility).
      | otherwise            -> stringify x

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a rep. Generic a rep => EncodeRep rep => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ genericEncodeJson
  , decodeJson : SPSettingsDecodeJson_ genericDecodeJson
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}
