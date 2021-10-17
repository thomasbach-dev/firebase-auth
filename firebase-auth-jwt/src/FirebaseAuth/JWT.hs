module FirebaseAuth.JWT
  ( validationSettings
  , GoogleJWKStore
  , newEmptyGoogleJWKStore
  , fetchGoogleJWKs
  , TokenUser(..)
  ) where

import qualified Crypto.JOSE       as JOSE
import qualified Crypto.JOSE.Types as JOSE
import qualified Crypto.JWT        as JOSE
import qualified Data.Aeson        as A
import qualified Data.Map          as M
import qualified Data.Text         as T

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Lens           ((&), (.~), (^.))
import Control.Monad          (forM, when)
import Control.Monad.Except   (MonadError, runExcept, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String            (IsString (..))
import Data.Time
    (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import GHC.Generics           (Generic)
import Network.HTTP.Simple    (getResponseBody, httpJSON)

import qualified FirebaseAuth.JWT.Internal as Internal

type GoogleJWKs = M.Map T.Text JOSE.JWK
type GoogleJWKStore = TVar (UTCTime, GoogleJWKs)


validationSettings :: String -- ^ Your firebase project id
                   -> JOSE.JWTValidationSettings
validationSettings projectId =
    JOSE.defaultJWTValidationSettings (== fromString projectId)
    & JOSE.jwtValidationSettingsIssuerPredicate .~ (uri ==)
  where
    uri = fromString ("https://securetoken.google.com/" <> projectId)

newEmptyGoogleJWKStore :: IO GoogleJWKStore
newEmptyGoogleJWKStore = newTVarIO (UTCTime (fromGregorian 1970 1 1) 0, mempty)

instance (MonadIO m, MonadError JOSE.JWTError m)
         => JOSE.VerificationKeyStore m (JOSE.JWSHeader ()) s GoogleJWKStore where
  getVerificationKeys hdr _ tvar = do
    kid <- case hdr ^. JOSE.kid of
             Nothing -> throwError $ JOSE.JWSError JOSE.JWSInvalidSignature
             Just k  -> pure $ k ^. JOSE.param
    (lastUpdateTime, keys) <- liftIO $ readTVarIO tvar
    case M.lookup kid keys of
      Just jwk -> pure [jwk]
      Nothing  -> do
        now <- liftIO getCurrentTime
        when (addUTCTime updateSchedule lastUpdateTime > now) $
          -- Too early to check for new keys!
          throwError $ JOSE.JWSError JOSE.JWSInvalidSignature
        eitherMap <- runExceptT fetchGoogleJWKs
        case eitherMap of
          Left _      -> throwError $ JOSE.JWSError JOSE.NoUsableKeys
          Right newKeys -> do
            liftIO . atomically $ writeTVar tvar (now, newKeys)
            case M.lookup kid newKeys of
              Just jwk -> pure [jwk]
              Nothing  -> throwError $ JOSE.JWSError JOSE.JWSInvalidSignature
    where
      updateSchedule = nominalDay

fetchGoogleJWKs :: (MonadError String m, MonadIO m) => m GoogleJWKs
fetchGoogleJWKs = do
 publicKeys <- fetchGooglePublicKeys
 forM publicKeys $ \pem ->do
   x509 <- pemToX509 pem
   x509ToJWK x509

type PEM = T.Text

fetchGooglePublicKeys :: MonadIO m => m (M.Map T.Text PEM)
fetchGooglePublicKeys = getResponseBody <$> httpJSON publicKeyEndpoint
  where
    publicKeyEndpoint =
      "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"

-- | A bit clumsy, but does the job for now.
pemToX509 :: MonadError String m => PEM -> m JOSE.Base64X509
pemToX509 = either throwError pure
          . A.eitherDecode . A.encode
          . T.concat . init . tail . T.lines

x509ToJWK :: (MonadError String m) => JOSE.Base64X509 -> m JOSE.JWK
x509ToJWK (JOSE.Base64X509 c) = either (throwError . show) pure jwk
  where
    jwk :: Either JOSE.Error JOSE.JWK
    jwk = runExcept $ JOSE.fromX509Certificate c

data TokenUser = TokenUser { tuUserId        :: T.Text
                           , tuEmail         :: T.Text
                           , tuEmailVerified :: Bool
                           } deriving (Eq, Generic, Show)

instance A.ToJSON TokenUser where
  toJSON = A.genericToJSON Internal.jsonOptionsSkipLowerCaseThenSnakeCase
  toEncoding = A.genericToEncoding Internal.jsonOptionsSkipLowerCaseThenSnakeCase

instance A.FromJSON TokenUser where
  parseJSON = A.genericParseJSON Internal.jsonOptionsSkipLowerCaseThenSnakeCase
