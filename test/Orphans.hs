module Orphans where

import Data.Hashable
import CryptoDepth.Types        (Amount)
import CryptoDepth.Exchange     (PathInfo)
import Data.Tagged              (Tagged)


instance Hashable (Tagged slippage (Amount numeraire))
instance Hashable (Amount numeraire)
instance Hashable (PathInfo numeraire slippage)
