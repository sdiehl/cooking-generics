module Test where

data PlatonicSolid
  = Tetrahedron
  | Cube
  | Octahedron
  | Dodecahedron
  | Icosahedron

data Person = Person
  { firstName       :: String
  , lastName        :: String
  , age             :: Int
  , height          :: Float
  , phoneNumber     :: String
  , flavor          :: String
  } deriving (Show)

data Point a b = Point a b

data T
  = T1 { a :: Int, b :: Float }
  | T2 { c :: Int, d :: Double }

data Hash a b = a :. b

data List a = Nil | Cons a (List a)

newtype Meters = Meters Int

newtype Id  x = MkId x
newtype Fix f = MkFix (f (Fix f))

data Void
