
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveTraversable #-}

-- | New paged APIs in GitHub return the items in an object with a
-- field with a type-dependent name.
--
-- In order to work well with the paging machinery in this library,
-- this module introduces a newtype 'Collection' that represents
-- collections that are returned this way.
--
-- The name of the collection of a type is defined by the
-- 'collectionName' method.
module GitHub.Data.Collection where

import GitHub.Internal.Prelude
import Data.Proxy
import qualified Data.Text as T

class CollectionName a where
  -- | Name used for this item when used in paging output, typically
  -- lowercase plural.
  collectionName :: proxy a -> Text

-- | A 'Vector' of items that are returned in an object with a
-- type-dependent field name.
newtype Collection a = Collection { collectionItems :: Vector a }
  deriving (Show, Data, Typeable, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (Collection a) where rnf = genericRnf
instance Binary a => Binary (Collection a)

instance Semigroup (Collection a) where
  Collection a <> Collection b = Collection (a <> b)
instance Monoid (Collection a) where
  mempty = Collection mempty
  mappend = (<>)

instance (CollectionName a, FromJSON a) => FromJSON (Collection a) where
  parseJSON = let name = collectionName (Proxy :: Proxy a)
              in withObject ("Collection of " <> T.unpack name) $ \o -> Collection <$> o .: name
