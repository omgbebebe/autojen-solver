{-# language OverloadedStrings #-}
module Autojen.Plugins.Parsers
( parsePluginsList
) where

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- versions
import Data.Versions
-- autojen-solver
import Autojen.Types

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
