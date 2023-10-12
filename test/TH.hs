-- | Template Haskell seems picky about compilation stages. The code here must
-- be defined in a module separate from the one it's being used in.

module TH (thVer) where

import qualified Data.Text as T
import           Data.Versions
import           Language.Haskell.TH (Exp, Q)
import           Language.Haskell.TH.Syntax (lift)

---

-- | Parse a `Versioning` at compile time.
thVer :: T.Text -> Q Exp
thVer nm =
  case versioning nm of
    Left err -> fail (errorBundlePretty err)
    Right v  -> lift v

