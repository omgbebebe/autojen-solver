{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Autojen.Plugins.PPrinters (
  asPlain
 ,asJson
 ,asNix
) where

-- aeson
import Data.Aeson
-- bytestring
import qualified Data.ByteString.Lazy as LBS
-- hnix
import qualified Nix.Expr as Nix
import Nix.Expr.Shorthands ((@@))
import qualified Nix.Pretty as Nix
-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- version
import Data.Versions (prettyV)
-- autojen-solver
import Autojen.Types

asPlain :: [Package] -> Text
asPlain pkgs = T.unlines $ map (\Package{..} -> pkgName <> ":" <> prettyV pkgVersion) pkgs

asJson :: [Package] -> Text
asJson pkgs = T.decodeUtf8 . LBS.toStrict . encode $ pkgs

asNix :: [Package] -> Text
asNix pkgs = T.pack . show . Nix.prettyNix $ Nix.mkFunction args $ Nix.mkLets [mkJenkinsPlugin] res
  where
    args = Nix.mkParamset exprs False
    mkJenkinsPlugin =
      Nix.bindTo "mkJenkinsPlugin" $
        Nix.mkFunction (Nix.mkParamset
          [ ("name", Nothing)
          , ("src", Nothing)
          ]
          False) $
            Nix.mkSym "stdenv.mkDerivation" @@ Nix.mkNonRecSet
              [ Nix.inherit [ Nix.StaticKey "name"
                            , Nix.StaticKey "src"
                            ] Nix.nullPos
              , "phases" Nix.$= Nix.mkStr "installPhase"
              , "installPhase" Nix.$= Nix.mkStr "cp $src $out"
              ]
    res = Nix.mkNonRecSet $ map formatPackage pkgs
    exprs :: [(Text, Maybe Nix.NExpr)]
    exprs =
      [ ("stdenv", Nothing)
      , ("fetchurl", Nothing)
      ]

    fetchurl :: Package -> Nix.NExpr
    fetchurl Package{..} = Nix.mkSym "fetchurl" @@
      Nix.mkNonRecSet [ "url" Nix.$= Nix.mkStr pkgUrl
                      , "sha256" Nix.$= Nix.mkStr pkgSha256
                      ]

    mkBody :: Package -> Nix.NExpr
    mkBody p@Package{..} = Nix.mkSym "mkJenkinsPlugin" @@
      Nix.mkNonRecSet [ "name" Nix.$= Nix.mkStr pkgName
                      , "src" Nix.$= fetchurl p
                      ]

    formatPackage :: Package -> Nix.Binding Nix.NExpr
    formatPackage p@Package{..} = pkgName Nix.$= mkBody p
