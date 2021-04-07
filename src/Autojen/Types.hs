{-# language DeriveGeneric #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Autojen.Types where

-- base
import GHC.Generics
-- aeson
import Data.Aeson.Types
-- text
import Data.Text
-- versions
import Data.Versions

instance FromJSON Versioning where
  parseJSON (String v) = case versioning v of
    Left e -> mempty
    Right x -> pure x
  parseJSON err =
    prependFailure "parsing Versioning failed, "
      (typeMismatch "String" err)

instance ToJSON Version where
  toJSON v = String (prettyVer v)

instance ToJSON Versioning where
  toJSON v = String (prettyV v)

instance FromJSON Version where
  parseJSON (String v) = case version v of
    Left e -> mempty
    Right x -> pure x
  parseJSON err =
    prependFailure "parsing Version failed, "
      (typeMismatch "String" err)

type Url = Text
type Name = Text

data PackageDependency = PackageDependency
  { pkgDepName :: Name
  , pkgDepMinVersion :: Versioning
  , pkgDepIsOptional :: Bool
  } deriving (Eq, Show, Generic)

instance FromJSON PackageDependency where
  parseJSON = withObject "dependency" $ \v -> PackageDependency
    <$> v .: "name"
    <*> v .: "version"
    <*> v .: "optional"

instance ToJSON PackageDependency

data Package = Package
  { pkgName :: Name
  , pkgVersion :: Versioning
  , pkgBuildDate :: Text
  , pkgDependencies :: [PackageDependency]
  , pkgRequiredCore :: Version
  , pkgSha1 :: Text
  , pkgSha256 :: Text
  , pkgUrl :: Url
  } deriving (Eq, Show, Generic)

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v -> Package
    <$> v .: "name"
    <*> v .: "version"
    <*> v .: "buildDate"
    <*> v .: "dependencies"
    <*> v .: "requiredCore"
    <*> v .: "sha1"
    <*> v .: "sha256"
    <*> v .: "url"

{- No isomorphism here!
-- we just drop the dependencies field for output simplification
-- actual it needs to be converted to a special `output` type
-- or something like that
-- maybe it will be implemented later in case of need
-}
instance ToJSON Package where
  toJSON Package{..} = object
    [ "name" .= pkgName
    , "version" .= pkgVersion
    , "buildDate" .= pkgBuildDate
    , "requiredCore" .= pkgRequiredCore
    , "sha1" .= pkgSha1
    , "sha256" .= pkgSha256
    , "url" .= pkgUrl
    ]
