{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Foreign (Foreign, typeOf)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Global (encodeURIComponent)
import Global.Unsafe (unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)


-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_
  (forall a rep. Generic a rep => GenericEncode rep => a -> Foreign)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_
  (forall a rep. Generic a rep => GenericDecode rep => Foreign -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_
  (forall a rep. Generic a rep => GenericEncode rep => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_
  (forall a rep. Generic a rep => GenericEncode rep => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a rep. Generic a rep => GenericEncode rep => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a rep. Generic a rep => GenericEncode rep => a -> URLPiece
gDefaultEncodeHeader v = case genericEncode defaultOptions v of
    s | typeOf s == "string" -> unsafeCoerce s -- Special case string - just use it as is (http-api-data compatibility).
      | otherwise            -> unsafeStringify s

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a rep. Generic a rep => GenericEncode rep => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ (genericEncode defaultOptions)
  , decodeJson : SPSettingsDecodeJson_
      (lmap show <<< runExcept <<< genericDecode defaultOptions)
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}
