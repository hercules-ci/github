{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}
module GitHub.Data.Apps where

import GitHub.Data.Definitions
import GitHub.Data.Id
import GitHub.Data.Collection
import GitHub.Internal.Prelude
import Data.Map(Map)

data App = App
  { appId :: Id App
  , appOwner :: SimpleOwner
  , appName :: Text
  -- FIXME: more
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData App where rnf = genericRnf
instance Binary App

instance FromJSON App where
  parseJSON = withObject "App" $ \o -> App
    <$> o .: "id"
    <*> o .: "owner"
    <*> o .: "name"

data Installation = Installation
  { installationId :: Id Installation
  , installationAccount :: SimpleOwner
  , installationAccessTokensUrl :: Text
  , installationRepositoriesUrl :: Text
  , installationHtmlUrl :: Text
  , installationAppId :: Id App
  , installationTargetId :: Id Owner
  , installationTargetType :: OwnerType
  , installationPermissions :: Map Text Text
  , installationEvents :: Vector Text -- FIXME
  , installationSingleFileName :: Maybe Text
  , installationRepositorySelection :: Text
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON Installation where
  parseJSON = withObject "Installation" $ \o -> Installation
    <$> o .: "id"
    <*> o .: "account"
    <*> o .: "access_tokens_url"
    <*> o .: "repositories_url"
    <*> o .: "html_url"
    <*> o .: "app_id"
    <*> o .: "target_id"
    <*> o .: "target_type"
    <*> o .: "permissions"
    <*> o .: "events"
    <*> o .: "single_file_name"
    <*> o .: "repository_selection"

instance CollectionName Installation where
  collectionName _ = "installations"


data InstallationToken = InstallationToken
  { installationTokenToken :: Text
  , installationTokenExpiresAt :: UTCTime
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON InstallationToken where
  parseJSON = withObject "Installation" $ \o -> InstallationToken
    <$> o .: "token"
    <*> o .: "expires_at"
