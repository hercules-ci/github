-- | See https://developer.github.com/v3/apps/installations/
module GitHub.Endpoints.Apps.Installations where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | See https://developer.github.com/v3/apps/installations/#list-repositories-accessible-to-the-user-for-an-installation
userInstallationRepositoriesR :: Id Installation -> FetchCount -> Request 'RA (Collection Repo)
userInstallationRepositoriesR instId = generalizedPagedQuery ["user", "installations", toPathPart instId, "repositories"] []

-- | See https://developer.github.com/v3/apps/installations/#list-repositories-accessible-to-the-user-for-an-installation
userInstallationRepositories' :: Auth -> Id Installation -> IO (Either Error (Collection Repo))
userInstallationRepositories' auth a = executeRequest auth $ userInstallationRepositoriesR a FetchAll

-- | See https://developer.github.com/v3/apps/installations/#list-repositories
installationRepositoriesR :: FetchCount -> Request 'AA (Collection Repo)
installationRepositoriesR = generalizedPagedQuery ["installation", "repositories"] []

-- | See https://developer.github.com/v3/apps/installations/#list-repositories
installationRepositories' :: Auth -> IO (Either Error (Collection Repo))
installationRepositories' auth = executeRequest auth $ installationRepositoriesR FetchAll
