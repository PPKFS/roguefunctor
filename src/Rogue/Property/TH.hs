module Rogue.Property.TH where
{-}
import Rogue.Prelude

import Data.Text (replace)
import Language.Haskell.Exts.Extension ( Extension(..), KnownExtension(..), Language(..) )
import Language.Haskell.Exts.Parser ( defaultParseMode, ParseMode(..) )
import Language.Haskell.Meta ( parseDecsWithMode )
import Language.Haskell.TH (Name, Q, Dec, nameBase)
import Data.List ((\\))


universeSans
  :: Bounded x
  => Enum x
  => Ord x
  => [x]
  -> [x]
universeSans x = universe \\ x

data SpecificsFunctions =
  GetX
  | SetX
  | ModifyX
  deriving stock (Show, Eq, Enum, Ord, Generic, Bounded)

myDefaultParseMode :: ParseMode
myDefaultParseMode = defaultParseMode
  { parseFilename = []
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension [DataKinds, ExplicitForAll, ScopedTypeVariables ]
  }

-- | Generate 0-3 of @getPropMaybe@, @setProp@, and @modifyProp@.
makeSpecificsWithout ::
  [SpecificsFunctions]
  -> Name
  -> Q [Dec]
makeSpecificsWithout l prop = do
  v <- mapM (makePropertyFunction prop) (universeSans l)
  return $ join v

makeSpecifics ::
  Name
  -> Q [Dec]
makeSpecifics = makeSpecificsWithout []

-- | Generate one of @getPropMaybe@, @setProp@, and @modifyProp@.
makePropertyFunction :: Name -> SpecificsFunctions -> Q [Dec]
makePropertyFunction n sf = do
  return $ (case sf of
    GetX -> replaceTH
      "getXSUBHEREMaybe :: (HasSpecifics o, MayHaveProperty (Specifics o) XSUBHERE) => o -> Maybe XSUBHERE\ngetXSUBHEREMaybe = defaultPropertyGetter"
    SetX -> replaceTH
      "setXSUBHERE :: (HasID o, HasSpecifics o, MayHaveProperty (Specifics o) XSUBHERE, ObjectQuery o :> es) => o -> XSUBHERE-> Eff es ()\nsetXSUBHERE = defaultPropertySetter"
    ModifyX -> replaceTH
      "modifyXSUBHERE :: (HasID o, HasSpecifics o, MayHaveProperty (Specifics o) XSUBHERE, ObjectQuery o :> es) => o -> (XSUBHERE -> XSUBHERE) -> Eff es ()\nmodifyXSUBHERE = modifyProperty getXSUBHEREMaybe setXSUBHERE"
    ) (toText $ nameBase n)

replaceTH :: Text -> Text -> [Dec]
replaceTH y x = either (\x' -> [error $ toText x']) id (parseDecsWithMode myDefaultParseMode $ toString $ replace "XSUBHERE" x y)
-}