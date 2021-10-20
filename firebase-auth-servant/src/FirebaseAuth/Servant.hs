{-# OPTIONS_GHC -Wno-orphans #-}
module FirebaseAuth.Servant
  ( FirebaseAuth
  , authHandler
  , AuthJWT.User(..)
  , AuthJWT.GoogleJWKStore
  , AuthJWT.newEmptyGoogleJWKStore
  ) where

import qualified Crypto.JOSE         as JOSE
import qualified Crypto.JWT          as JOSE
import qualified Data.ByteString     as BS
import qualified Servant.Auth.Server as SAS

import Control.Monad.Except             (runExceptT, throwError)
import Control.Monad.IO.Class           (liftIO)
import Data.ByteArray                   (constEq)
import Network.Wai                      (Request, requestHeaders)
import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Auth.JWT                 (FromJWT, ToJWT)
import Servant.Server                   (ServerError (errBody), err401)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Witch                            (from)

import qualified FirebaseAuth.JWT as AuthJWT

instance ToJWT AuthJWT.User
instance FromJWT AuthJWT.User

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
  eitherJws :: Either JOSE.Error (JOSE.CompactJWS JOSE.JWSHeader) <-
    runExceptT . JOSE.decodeCompact $ from token
  case eitherJws of
    Left err -> throwError err401 { errBody = "Error when decoding token: " <> from (show err) }
    Right jws -> do
      claims <- liftIO . runExceptT $
        JOSE.verifyClaims (AuthJWT.validationSettings projectId) keyStore jws
      case claims of
         Left err -> throwError
                       err401 { errBody = "Error when checking JWT claims: " <> from (show err) }
         Right c -> case SAS.decodeJWT c of
           Left err   -> throwError
                           err401 { errBody = "Error when parsing information from token: "
                                              <> from (show err)}
           Right user -> return user
