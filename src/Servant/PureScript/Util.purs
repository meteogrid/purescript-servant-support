module Servant.PureScript.Util where

import Prelude
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Encode.Generic.Rep (class EncodeRep)
import Data.Argonaut.Decode.Generic.Rep (class DecodeRep)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (makeAjaxError, AjaxError, ErrorDescription(DecodingError, ParsingError, UnexpectedHTTPStatus), AjaxRequest)
import Servant.PureScript.Settings (SPSettings_(SPSettings_), SPSettingsToUrlPiece_(SPSettingsToUrlPiece_), SPSettingsEncodeHeader_(SPSettingsEncodeHeader_))

-- | Get the result of a request.
-- |
-- | Reports an error if status code is non success or decoding fails. The taken AjaxRequest is only for error reporting.
getResult
  :: forall a m rep
   . Generic a rep
  => DecodeRep rep
  => MonadError AjaxError m
  => AjaxRequest
  -> (Json -> Either String a)
  -> AffjaxResponse String -> m a
getResult req' decode resp = do
  let stCode = case resp.status of StatusCode code -> code
  fVal <- if stCode >= 200 && stCode < 300
            then pure resp.response
            else throwError $ makeAjaxError req' (UnexpectedHTTPStatus resp)
  jVal <- throwLeft <<< lmap (reportRequestError req' ParsingError fVal) <<< jsonParser $ fVal
  throwLeft <<< lmap (reportRequestError req' DecodingError (unsafeStringify jVal)) <<< decode $ jVal

throwLeft :: forall a e m. MonadError e m => Either e a -> m a
throwLeft (Left e) = throwError e
throwLeft (Right a) = pure a


encodeListQuery :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> Array a -> String
encodeListQuery opts'@(SPSettings_ opts) fName = intercalate "&" <<< map (encodeQueryItem opts' fName)

-- | The given name is assumed to be already escaped.
encodeQueryItem :: forall a b rep. Generic a rep => EncodeRep rep => SPSettings_ b -> String -> a -> String
encodeQueryItem opts'@(SPSettings_ opts) fName val = fName <> "=" <> encodeURLPiece opts' val

-- | Call opts.toURLPiece and encode the resulting string with encodeURIComponent.
encodeURLPiece :: forall a rep params. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeURLPiece (SPSettings_ opts) = case opts.toURLPiece of SPSettingsToUrlPiece_ f -> f

encodeHeader :: forall a params rep. Generic a rep => EncodeRep rep => SPSettings_ params -> a -> String
encodeHeader (SPSettings_ opts) = case opts.encodeHeader of SPSettingsEncodeHeader_ f -> f

reportRequestError :: AjaxRequest -> (String -> ErrorDescription) -> String -> String -> AjaxError
reportRequestError req' err source msg = makeAjaxError req' $ reportError err source msg

reportError :: forall err. (String -> err) -> String -> String  -> err
reportError err source msg = err $ msg <> ", source: '" <> source <> "'"
