{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Autojen.Plugins.Solvers
( resolvePlugins
) where
-- base
import Data.Maybe
import Data.List (nub)
-- aeson
import Data.Aeson
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- containers
import Data.Tree
-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- versions
import Data.Versions
-- autojen-solver
--import Autojen.Types (Package(..), Name)
import Autojen.Types

resolvePlugins :: ByteString -> Version -> ByteString -> [Package]
resolvePlugins pluginsDb jenVersion inputPlugins =
  let
    pkgs = case eitherDecodeStrict' pluginsDb of
      Left e -> error $ "failed to decode plugins database. " <> show e
      Right x -> x
    initialPlugins = parsePluginsList inputPlugins
    b = buildDeps pkgs jenVersion
    allDeps = nub $ concat $ map flatten $ unfoldForest b initialPlugins
  in map (
    \p -> case findPackage p jenVersion pkgs of
      Nothing -> error $ show $ "cannot find " <> fst p <> ":" <> prettyV (snd p) <> " for Jenkins " <> prettyVer jenVersion
      Just x -> x
    ) allDeps

parsePluginsList :: ByteString -> [(Name, Versioning)]
parsePluginsList ps = do
  map (
    \p ->
      let xs = T.splitOn ":" p
          (Right ver) = versioning (xs !! 1)
      in
        if length xs /= 2
        then error $ "failed to parse record: " <> show p <> ". The format is <plugin_name>:<plugin_version>"
        else (head xs, ver)
    ) $ T.lines . T.decodeUtf8 $ ps

-- utility
findMaxVer :: Name -> Maybe Version -> [Package] -> Maybe Versioning
findMaxVer pn jv pkgs =
  case pkgs' of
    [] -> Nothing
    xs -> Just (maximum xs)
  where
--    pkgs'' = filter (\p -> pkgName p == pn) pkgs
    pkgs'' = filter (\p -> pn == pkgName p) pkgs
    pkgs' = map pkgVersion $ case jv of
      Nothing -> pkgs''
      Just x -> filter (\p -> pkgRequiredCore p <= x) pkgs''

findPackage :: (Name, Versioning) -> Version -> [Package] -> Maybe Package
findPackage (pn, pv) jv pkgs = do
  case filter (\p -> pkgName p == pn && pkgVersion p == pv && pkgRequiredCore p <= jv) pkgs of
    [] -> Nothing
    (x:[]) -> Just x
    _ -> error $ "more than one package was found: " <> show pn <> ":" <> show pv

buildDeps :: [Package] -> Version -> (Name, Versioning) -> ((Name, Versioning), [(Name, Versioning)])
buildDeps pkgs jenVersion p@(pn,pv) = (p, deps)
  where
    pkg = case findPackage (pn,pv) jenVersion pkgs of
            Nothing -> error $ "cannot find package for " <> show pn <> show (prettyV pv)
            Just p -> p
    deps = map (
      \pd ->
        let pv = fromJust (findMaxVer (pkgDepName pd) (Just jenVersion) pkgs)
            p = fromJust (findPackage ((pkgDepName pd), pv) jenVersion pkgs)
        in (pkgName p, pkgVersion p)
      ) $ filter (\PackageDependency{..} -> not pkgDepIsOptional) (pkgDependencies pkg)
