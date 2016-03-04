module Dump where

import GHC
import GHC.Paths as Paths

import Name
import TyCon
import TypeRep
import DataCon
import HscTypes

import Text.Show.Pretty

-- Deconstructed datatypes.

data Datatype = Datatype
  { dataTypeName :: String
  , modName :: String
  , isNewtype :: Bool
  , datatype :: Data
  , recursive :: Bool
  } deriving (Show)

data Constructor = Constructor
  { conName :: String
  } deriving (Show)

data Selector = Selector
  { selName :: String
  } deriving (Show)

data Metadata
  = D Datatype
  | C Constructor
  | S Selector Data
  deriving (Show)

data Data
  = Sum [Data]
  | Product [Data]
  | M1 Metadata
  | K1 String
  | V1
  deriving (Show)

-- Deconstructor

deconstruct :: [TyCon] -> [Data]
deconstruct = fmap go
  where
    go x
      | isProduct x = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = Product (mkProduct x)
        , recursive    = isRecursiveTyCon x
        }

      | isVoid x = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = V1
        , recursive    = isRecursiveTyCon x
        }

      | otherwise = M1 $ D Datatype
        { dataTypeName = getOccString (tyConName x)
        , modName      = modString x
        , isNewtype    = isNewTyCon x
        , datatype     = Sum (mkProduct x)
        , recursive    = isRecursiveTyCon x
        }

mkRecord :: TyCon -> [Data]
mkRecord x = concatMap mkRProduct (tyConDataCons x)

mkProduct :: TyCon -> [Data]
mkProduct x = fmap go (tyConDataCons x)
  where
    go :: DataCon -> Data
    go x | isRecord x   = Product (mkRProduct x)
    go x | isDProduct x = Product (mkDProduct x)
    go x                = M1 $ C (Constructor (conNames x))

mkDProduct :: DataCon -> [Data]
mkDProduct xs = [K1 (showType x) | x <- dataConOrigArgTys xs]

mkRProduct :: DataCon -> [Data]
mkRProduct x = [M1 (S (Selector (getOccString fld)) ty) | (fld, ty) <- zip (fieldNames x) (mkDProduct x)]

modString :: TyCon -> String
modString = moduleNameString . moduleName . nameModule . tyConName

showType :: Type -> String
showType (TyVarTy tv)       = getOccString tv
showType (TyConApp tc tys)  = getOccString tc
showType (FunTy a b)        = showType a ++ "->" ++ showType b
showType (AppTy a b)        = showType a ++ " " ++ showType b
showType (ForAllTy _ a)     = showType a

fieldNames :: DataCon -> [FieldLabel]
fieldNames = dataConFieldLabels

conNames :: DataCon -> String
conNames = getOccString . dataConName

isRecord :: DataCon -> Bool
isRecord x = length (fieldNames x) > 0

isProduct :: TyCon -> Bool
isProduct = isProductTyCon

isDProduct :: DataCon -> Bool
isDProduct x = dataConRepArity x > 0

isVoid :: TyCon -> Bool
isVoid x = length (tyConDataCons x) == 0

main :: IO ()
main = do

  -- Inside the GHC Monad
  rep <- runGhc (Just Paths.libdir) $ do

    -- Spin up a GHC compiler environment
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags

    -- Make a dummy module to inject
    let mn = mkModuleName "Test"

    -- Make a dummy target
    addTarget Target {
      targetId = TargetModule mn
    , targetAllowObjCode = True
    , targetContents = Nothing
    }

    -- Run the GHC pipeline
    load LoadAllTargets
    modSum <- getModSummary mn
    p <- parseModule modSum
    t <- typecheckModule p

    -- Pluck out the module tycons after we're done type-checking
    DesugaredModule tcmod modguts <- desugarModule t
    let tycons = mg_tcs modguts

    -- Deconstruct all datatypes into their sums-of-products.
    return (deconstruct tycons)

  putStrLn (ppShow rep)
