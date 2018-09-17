-- | https://developer.github.com/v3/apps/
module GitHub.Endpoints.Apps where

import GitHub.Data.Collection
import GitHub.Data.Definitions
import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | See https://developer.github.com/v3/apps/#get-the-authenticated-github-app
appR :: Request 'AA App
appR = query ["app"] []

-- | See https://developer.github.com/v3/apps/#get-the-authenticated-github-app
app' :: Auth -> IO (Either Error App)
app' auth = executeRequest auth $ appR

-- | See https://developer.github.com/v3/apps/#create-a-new-installation-token
installationsAccessTokensR :: Id Installation -> Request 'AA InstallationToken
installationsAccessTokensR instId =
  appCommand Post ["installations", toPathPart instId, "access_tokens"] mempty

-- | See https://developer.github.com/v3/apps/#create-a-new-installation-token
installationsAccessTokens' :: Auth -> Id Installation -> IO (Either Error InstallationToken)
installationsAccessTokens' auth instId = executeRequest auth $ installationsAccessTokensR instId

-- | See https://developer.github.com/v3/apps/#list-installations-for-user
userInstallationsR :: FetchCount -> Request 'RA (Collection Installation)
userInstallationsR = generalizedPagedQuery ["user", "installations"] []

-- | See https://developer.github.com/v3/apps/#list-installations-for-user
userInstallations' :: Auth -> IO (Either Error (Collection Installation))
userInstallations' auth = executeRequest auth $ userInstallationsR FetchAll

