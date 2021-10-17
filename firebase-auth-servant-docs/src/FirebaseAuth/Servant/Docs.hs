{-# OPTIONS_GHC -Wno-orphans #-}
module FirebaseAuth.Servant.Docs () where

import qualified Servant.Docs as Docs

import Control.Lens  ((.~))
import Data.Function ((&))
import Data.Proxy    (Proxy (..))
import Servant       ((:>))

import qualified FirebaseAuth.Servant as AuthServant

-- | TODO Provide something meaningful here
instance Docs.HasDocs api => Docs.HasDocs (AuthServant.FirebaseAuth :> api) where
  docsFor _ (endpoint, action) = Docs.docsFor (Proxy @api) (endpoint, action')
    where
      action' = action & Docs.authInfo .~ []
