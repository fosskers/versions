-- | Template Haskell seems picky about compilation stages. The code here must
-- be defined in a module separate from the one it's being used in.

module TH where

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

-- | Parse a `Version` at compile time.
thV :: T.Text -> Q Exp
thV nm =
  case version nm of
    Left err -> fail (errorBundlePretty err)
    Right v  -> lift v

-- | Parse a `PVP` at compile time.
thP :: T.Text -> Q Exp
thP nm =
  case pvp nm of
    Left err -> fail (errorBundlePretty err)
    Right v  -> lift v
