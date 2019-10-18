module GitHub.Data.Membership where

import GitHub.Data.Definitions
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import qualified Data.Text as T
import Prelude ()


-- state field

data MembershipState = Active | Pending
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData MembershipState where rnf = genericRnf
instance Binary MembershipState

instance FromJSON MembershipState where
    parseJSON = withText "Membership state" $ \t ->
        case t of
          "pending" -> pure Pending
          "active"  -> pure Active
          _         -> fail $ "Unknown membership state: " ++ T.unpack t


-- role field

data MembershipRole = Admin | Member
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData MembershipRole where rnf = genericRnf
instance Binary MembershipRole

instance FromJSON MembershipRole where
  parseJSON = withText "Membership role" $ \t ->
      case t of
        "admin"  -> pure Admin
        "member" -> pure Member
        _        -> fail $ "Unknown membership role: " ++ T.unpack t


-- Membership object

data Membership = Membership
    { membershipState        :: !MembershipState
    , membershipRole         :: !MembershipRole
    , membershipOrganization :: !SimpleOrganization
    , membershipUser         :: !SimpleUser
    , membershipUrl          :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Membership where rnf = genericRnf
instance Binary Membership

instance FromJSON Membership where
    parseJSON = withObject "Membership" $ \o -> Membership
        <$> o .: "state"
        <*> o .: "role"
        <*> o .: "organization"
        <*> o .: "user"
        <*> o .: "url"


