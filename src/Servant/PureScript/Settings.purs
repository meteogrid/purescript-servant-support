{--
  This file contains code copied from the purescript-affjax project from slamdata.
  It is therefore licensed under Apache License version 2.0.
--}
module Servant.PureScript.Settings where


import Prelude
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Foreign (Foreign, readString)
import Data.Foreign.Class (class Encode, class Decode, encode, decode)
import Data.Either (Either(Right))
import Global (encodeURIComponent)
import Global.Unsafe (unsafeStringify)


-- encodeJson, decodeJson, toURLPiece have to be wrapped in newtype. See:
-- https://github.com/purescript/purescript/issues/1957

newtype SPSettingsEncodeJson_ = SPSettingsEncodeJson_ (forall a. Encode a => a -> Foreign)
newtype SPSettingsDecodeJson_ = SPSettingsDecodeJson_ (forall a. Decode a => Foreign -> Either String a)
newtype SPSettingsToUrlPiece_ = SPSettingsToUrlPiece_ (forall a. Encode a => a -> URLPiece)
newtype SPSettingsEncodeHeader_ = SPSettingsEncodeHeader_ (forall a. Encode a => a -> URLPiece)

newtype SPSettings_ params = SPSettings_ {
    encodeJson :: SPSettingsEncodeJson_
  , decodeJson :: SPSettingsDecodeJson_
  , toURLPiece :: SPSettingsToUrlPiece_
  , encodeHeader :: SPSettingsEncodeHeader_
  , params :: params
  }

type URLPiece = String

-- | Just use the robust JSON format.
gDefaultToURLPiece :: forall a. Encode a => a -> URLPiece
gDefaultToURLPiece = gDefaultEncodeHeader

-- | Just use the robust JSON format.
gDefaultEncodeHeader :: forall a. Encode a => a -> URLPiece
gDefaultEncodeHeader v = case encode v of
  j | Right s <- runExcept (readString j) ->  s -- Special case string - just use it as is (http-api-data compatibility).
    | otherwise                            -> unsafeStringify j

-- | Full encoding based on gDefaultToURLPiece
gDefaultEncodeURLPiece :: forall a. Encode a => a -> URLPiece
gDefaultEncodeURLPiece = encodeURIComponent <<< gDefaultToURLPiece


defaultSettings :: forall params. params -> SPSettings_ params
defaultSettings params = SPSettings_ {
    encodeJson : SPSettingsEncodeJson_ encode
  , decodeJson : SPSettingsDecodeJson_ (lmap show <<< runExcept <<< decode)
  , toURLPiece : SPSettingsToUrlPiece_ gDefaultToURLPiece
  , encodeHeader : SPSettingsEncodeHeader_ gDefaultEncodeHeader
  , params : params
}
