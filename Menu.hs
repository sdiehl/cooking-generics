{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Menu where

import Data.Text
import Data.Proxy
import GHC.Generics

import Text.Show.Pretty (ppShow)

-- Datatypes

data Pie = Pie
  { filling :: Filling
  , topping :: Maybe Topping
  } deriving (Show, Generic, Menu)

data Crisp = Crisp
  { contents :: Filling
  , temperature :: Temperature
  } deriving (Show, Generic, Menu)

data Filling = Apple | Cherry | Pumpkin
  deriving (Show, Generic, Menu)

data Topping = IceCream | WhipCream
  deriving (Show, Generic, Menu)

data Temperature = Warm | Cold
  deriving (Show, Generic, Menu)

data Item
  = Item Text [Item]
  | Variant Text [Item]
  | Choice Text
  deriving (Show, Generic)


-- Menu Classes

class Menu a where
  menu :: a -> [Item]
  default menu :: (Generic a, GMenu (Rep a)) => a -> [Item]
  menu _ = gmenu (Proxy :: Proxy a)


instance Menu a => Menu (Maybe a) where
  menu _ = [Choice (pack "AsIs")] ++ (menu (undefined :: a))

instance Menu a => Menu [a] where
  menu _ = (menu (undefined :: a))

instance (Menu a, Menu b) => Menu (a,b) where
  menu _ = menu (undefined :: a) ++ menu (undefined :: b)

-- Generic Menu
class GMenu a where
  gopts :: Proxy a -> [Item]

-- Datatype
instance GMenu f => GMenu (M1 D x f) where
  gopts _ = gopts (Proxy :: Proxy f)

-- Constructor Metadata
instance (GMenu f, Constructor c) => GMenu (M1 C c f) where
  gopts x
    | conIsRecord (undefined :: t c f a) =
      [Item (pack (conName m)) (gopts (Proxy :: Proxy f))]

    | otherwise = [Choice (pack (conName m))]
    where m = (undefined :: t c f a)

-- Selector Metadata
instance (GMenu f, Selector c) => GMenu (M1 S c f) where
  gopts _ = [Variant (pack (selName m)) (gopts (Proxy :: Proxy f))]
    where m = (undefined :: t c f a)

-- Constructor Paramater
instance (GMenu (Rep f), Menu f) => GMenu (K1 R f) where
  gopts _ = menu (undefined :: f)

-- Sum branch
instance (GMenu a, GMenu b) => GMenu (a :+: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Product branch
instance (GMenu a, GMenu b) => GMenu (a :*: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Void branch
instance GMenu U1 where
  gopts _ = []

gmenu :: forall a. (Generic a, GMenu (Rep a)) => Proxy a -> [Item]
gmenu _ = gopts (Proxy :: Proxy (Rep a))

sample1 :: IO ()
sample1 = putStrLn $ ppShow $ menu (undefined :: Pie)

sample2 :: IO ()
sample2 = putStrLn $ ppShow $ menu (undefined :: (Pie, Crisp))
