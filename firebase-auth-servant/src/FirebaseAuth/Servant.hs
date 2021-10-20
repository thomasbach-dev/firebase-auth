{-# OPTIONS_GHC -Wno-orphans #-}
module FirebaseAuth.Servant
  ( FirebaseAuth
  , authHandler
  , AuthJWT.User(..)
  , AuthJWT.GoogleJWKStore
  , AuthJWT.newEmptyGoogleJWKStore
  ) where

import qualified Crypto.JOSE          as JOSE
import qualified Crypto.JWT           as JOSE
import qualified Data.Aeson           as A
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T

import Control.Lens                     ((^.))
import Control.Monad.Except             (MonadError, runExceptT, throwError)
import Control.Monad.IO.Class           (MonadIO, liftIO)
import Data.ByteArray                   (constEq)
import Data.List                        (intercalate)
import Network.Wai                      (Request, requestHeaders)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Server                   (ServerError (errBody), err401)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Witch                            (from)

import qualified FirebaseAuth.JWT as AuthJWT

type FirebaseAuth = AuthProtect "firebase-auth"

type instance AuthServerData FirebaseAuth = AuthJWT.User

authHandler :: String  -- ^ Your firebase project id
            -> AuthJWT.GoogleJWKStore  -- ^ A key store
            -> AuthHandler Request AuthJWT.User
authHandler projectId keyStore = mkAuthHandler $ \req -> do
  token <- case lookup "Authorization" $ requestHeaders req of
    Nothing -> throwError err401 { errBody = "Missing Authorization header!" }
    Just authHdr -> do
      let bearer = "Bearer "
          (mbearer, rest) = BS.splitAt (BS.length bearer) authHdr
      if mbearer `constEq` bearer
        then return rest
        else throwError err401 { errBody = "Expected Bearer authorization type!" }
  eitherUser <- runExceptT $ do
    jwt <- decodeToken (from token)
    claims <- verifyClaims (AuthJWT.validationSettings projectId) keyStore jwt
    parseUser claims
  case eitherUser of
    Left err -> throwError err401 { errBody = from err }
    Right u  -> return u

verifyClaims :: (MonadError String m, MonadIO m)
             => JOSE.JWTValidationSettings
             -> AuthJWT.GoogleJWKStore
             -> JOSE.SignedJWT
             -> m JOSE.ClaimsSet
verifyClaims validationSettings keyStore jwt = do
  eitherClaims <- liftIO . runExceptT $ JOSE.verifyClaims validationSettings keyStore jwt
  case eitherClaims of
     Left err     -> throwError $ "Error when checking JWT claims: " <> from (show err)
     Right claims -> return claims

decodeToken :: MonadError String m => BL.ByteString -> m JOSE.SignedJWT
decodeToken token = do
  eitherJws <- runExceptT $ JOSE.decodeCompact token
  case eitherJws of
     Left (err :: JOSE.Error) -> throwError $ "Error when decoding token: " <> show err
     Right jws                -> return jws

parseUser :: MonadError String m => JOSE.ClaimsSet -> m AuthJWT.User
parseUser claims = do
  eitherUser <- runExceptT $ AuthJWT.User
                              <$> lookupOrThrowText "user_id" keyValues
                              <*> lookupOrThrowText "email" keyValues
                              <*> lookupOrThrowBool "email_verified" keyValues
  case eitherUser of
    Left err -> throwError $ "Error when parsing User from JWT: " <> err
    Right u  -> return u
  where
    keyValues = claims ^. JOSE.unregisteredClaims

lookupOrThrowText :: MonadError String m => T.Text -> HM.HashMap T.Text A.Value -> m T.Text
lookupOrThrowText key claims = do
  value <- lookupOrThrow key claims
  case value of
    A.String x -> return x
    v          -> throwLookupError "string" key v

lookupOrThrowBool :: MonadError String m => T.Text -> HM.HashMap T.Text A.Value -> m Bool
lookupOrThrowBool key claims = do
  value <- lookupOrThrow key claims
  case value of
    A.Bool x -> return x
    v        -> throwLookupError "bool" key v

throwLookupError :: (MonadError String m , Show k, Show v) => String -> k -> v -> m a
throwLookupError expectedType key value = throwError $ " " `intercalate` body
  where
    body = [ "Expected type", expectedType, "when looking up key ", show key <> "."
           , "Found:", show value <> "."
           ]

lookupOrThrow :: MonadError String m => T.Text -> HM.HashMap T.Text A.Value -> m A.Value
lookupOrThrow key claims = case HM.lookup key claims of
                             Just value -> return value
                             Nothing    -> throwError $ "No such key: " <> show key
