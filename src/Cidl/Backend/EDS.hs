module Cidl.Backend.EDS where

import Cidl.Dict
import Ivory.Artifact

edsBackend :: [Dict] -> String -> String -> [Artifact]
edsBackend dicts pkgname namespace_raw =
  [ artifactString
      (dictName d ++ ".eds")
      (show d)
    | d <- dicts
  ]
