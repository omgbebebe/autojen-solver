{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Autojen.Plugins.Solvers
( resolvePlugins
) where
-- base
import Data.Maybe
import Data.List (nub, sortOn)
-- aeson
import Data.Aeson
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- containers
import qualified Data.Map as M
import Data.Tree
-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- versions
import Data.Versions
-- autojen-solver
import Autojen.Plugins.Parsers (parsePluginsList)
import Autojen.Types

resolveDoubles :: [Package] -> [Package]
resolveDoubles pkgs =
  let
    m = foldr (
      \p acc -> case M.lookup (pkgName p) acc of
        Just x -> let ps = filter (\p -> pkgName p == pkgName x) pkgs
                      maxVer = head $ reverse $ sortOn pkgVersion ps
                  in M.insert (pkgName p) maxVer acc
        Nothing -> M.insert (pkgName p) p acc
      ) M.empty pkgs
  in M.elems m

resolvePlugins :: ByteString -> Version -> ByteString -> [Package]
resolvePlugins pluginsDb jenVersion inputPlugins =
  let
    pkgs = case eitherDecodeStrict' pluginsDb of
      Left e -> error $ "failed to decode plugins database. " <> show e
      Right x -> x
    initialPlugins = parsePluginsList inputPlugins
    b = buildDeps pkgs jenVersion
    allDeps = nub $ concat $ map flatten $ unfoldForest b initialPlugins
  in resolveDoubles $ map (
    \p -> case findPackage p jenVersion pkgs of
      Nothing -> error $ show $ "cannot find " <> fst p <> ":" <> prettyV (snd p) <> " for Jenkins " <> prettyVer jenVersion
      Just x -> x
    ) allDeps

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
      ) $ pkgDependencies pkg --filter (\PackageDependency{..} -> not pkgDepIsOptional) (pkgDependencies pkg)
