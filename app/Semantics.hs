module Semantics
  (
  ) where

import KBYDoc

import qualified System.FilePath as SysPath
import Path
import Error
import Options

--Check if url exists, and resolve full path if it does.
resolveUrl :: Options -> URL -> URL
resolveUrl opts (AssetRef src) = AssetRef src
--right now do nothing for PageRef, will need to pass src dir as well to check these.
resolveUrl opts (PageRef src) = PageRef src
--cant really do anything here right now... maybe ping webpage or something?
resolveUrl opts (RemoteRef src) = RemoteRef src
