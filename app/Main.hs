{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}
module Main where
-- base
import Data.String (IsString(..))
import System.IO (withFile, IOMode(..))
-- bytestring
import qualified Data.ByteString as BS
-- optparse-applicative
import Options.Applicative
-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- versioning
import Data.Versions (Version, version, prettyV)
-- autojen-solver
import Autojen.Plugins.Solvers (resolvePlugins)
import Autojen.Types (Package(..))

data Config = Config
  { pluginsListFile :: FilePath
  , jenkinsVersion :: Version
  , pluginsJson :: FilePath
  , verbose :: Bool
  } deriving (Eq, Show)

instance IsString Version where
  fromString x = case version (T.pack x) of
    Left e -> error $ "failed to convert " <> x <> " to version. (" <> show e <> ")"
    Right v -> v

run :: Config -> IO ()
run Config{..} = do
  pluginsDatabase <- withFile pluginsJson ReadMode BS.hGetContents
  inputPlugins <- withFile pluginsListFile ReadMode BS.hGetContents
  let allWithDeps = resolvePlugins pluginsDatabase jenkinsVersion inputPlugins
      --result = map (\Package{..} -> pkgName <> "\t" <> prettyV pkgVersion <> "\t" <> pkgUrl) allWithDeps
      result = map (\Package{..} -> pkgName <> ":" <> prettyV pkgVersion) allWithDeps
  mapM_ T.putStrLn result

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Resolve plugins dependencies for given Jenkins version"
     <> header "Build an immutable Jenkins for great good" )

config :: Parser Config
config = Config
  <$> strOption
    (  long "plugins-list-file"
    <> short 'f'
    <> metavar "PLUGINS_LIST_FILE"
    <> showDefault
    <> value "plugins.list"
    <> help "Path to the file with list of plugins")
  <*> strOption
    (  long "jenkins-version"
    <> short 'j'
    <> metavar "TARGET_JENKINS_VERSION"
    <> help "Version of Jenkins Core")
  <*> strOption
    (  long "plugins-database"
    <> short 'f'
    <> metavar "PLUGINS_JSON_FILE"
    <> showDefault
    <> value "plugins.json"
    <> help "Path to the plugins.json")
  <*> switch
    (  long "verbose"
    <> short 'v'
    <> showDefault
    <> help "verbose output")
