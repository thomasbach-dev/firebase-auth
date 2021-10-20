{-# OPTIONS_GHC -Wno-orphans #-}
module FirebaseAuth.Servant.Docs () where

import qualified Servant.Docs as Docs

import Control.Lens  ((%~), (|>))
import Data.Function ((&))
import Data.Proxy    (Proxy (..))
import Servant       ((:>))

import qualified FirebaseAuth.Servant as AuthServant

instance Docs.HasDocs api => Docs.HasDocs (AuthServant.FirebaseAuth :> api) where
  docsFor _ (endpoint, action) = Docs.docsFor (Proxy @api) (endpoint, action')
    where
      action' = action & Docs.authInfo %~ (|> info)
      info = Docs.DocAuthentication intro reqData
      intro = "This endpoint requires authentication!"
      reqData = unwords
        [ "- an `Authorization` header"
        , "where the value starts with `Bearer: ` followed by a JWT token;"
        , "the token has to be issued by Google Firebase;"
        ]

instance Docs.HasDocs api => Docs.HasDocs (AuthServant.OptionalFirebaseAuth :> api) where
  docsFor _ (endpoint, action) = Docs.docsFor (Proxy @api) (endpoint, action')
    where
      action' = action & Docs.authInfo %~ (|> info)
      info = Docs.DocAuthentication intro reqData
      intro = "This endpoint can be used with optional authentication!"
      reqData = unwords
        [ "- an optional `Authorization` header"
        , "where the value starts with `Bearer: ` followed by a JWT token;"
        , "the token has to be issued by Google Firebase;"
        , "if the authorization header is present, but invalid,"
        , "the endpoint will return a 401;"
        ]
