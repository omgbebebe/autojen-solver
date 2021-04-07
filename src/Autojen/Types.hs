{-# language OverloadedStrings #-}
module Autojen.Types where

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
  } deriving (Eq, Show)

instance FromJSON PackageDependency where
  parseJSON = withObject "dependency" $ \v -> PackageDependency
    <$> v .: "name"
    <*> v .: "version"
    <*> v .: "optional"

data Package = Package
  { pkgName :: Name
  , pkgVersion :: Versioning
  , pkgBuildData :: Text
  , pkgDependencies :: [PackageDependency]
  , pkgRequiredCore :: Version
  , pkgSha1 :: Text
  , pkgSha256 :: Text
  , pkgUrl :: Url
  } deriving (Eq, Show)

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
