{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeFamilies, RankNTypes #-}
-- | Lenses for working with YAML structures.
module Data.Yaml.YamlLight.Lens (
   -- * Traversals
   nth, key, key', 
   -- * Yaml parsing prism
   _Yaml, AsYaml(..),
   -- * Numeric parsers
   yamlInt, yamlReal) where
import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lex.Integral as I
import qualified Data.ByteString.Lex.Fractional as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Traversable (sequenceA)
import Data.Yaml.YamlLight

-- $setup
-- >>> :set -XOverloadedStrings

-- | The two indexable types of YAML data are sequences and mappings.
data YamlIx = ArrIx Int | ObjIx YamlLight

type instance Index YamlLight = YamlIx
type instance IxValue YamlLight = YamlLight

instance Ixed YamlLight where
  ix k@(ArrIx i) f (YSeq xs) | i < 0 = pure (YSeq xs)
                             | otherwise = YSeq <$> go xs i where
    go [] _ = pure []
    go (y:ys) 0 = (:ys) <$> indexed f k y
    go (y:ys) i' = (y:) <$> (go ys $! i' - 1)
  ix k@(ObjIx k') f (YMap m) = case Map.lookup k' m of
    Just v -> YMap . flip (Map.insert k') m <$> indexed f k v
    Nothing -> pure (YMap m)
  ix _ _ y = pure y

instance At YamlLight where
  at k@(ObjIx k') f (YMap m) = YMap . aux <$> indexed f k mv
    where aux Nothing = maybe m (const (Map.delete k' m)) mv
          aux (Just v) = Map.insert k' v m
          mv = Map.lookup k' m
  at k f y = const y <$> indexed f k Nothing

instance Each YamlLight YamlLight YamlLight YamlLight where
  each f (YSeq xs) = YSeq <$> traverse (uncurry $ indexed f)
                                       (zip (map ArrIx [0..]) xs)
  each f (YMap m) = YMap <$> sequenceA (Map.mapWithKey (indexed f . ObjIx) m)
  each _ y = pure y

instance Plated YamlLight where
  plate f (YSeq xs) = YSeq <$> traverse f xs
  plate f (YMap m) = YMap <$> traverse f m
  plate _f y = pure y

noRemainder :: (a, ByteString) -> Maybe a
noRemainder (x, bs) = if BC.null bs then Just x else Nothing

-- | Try to parse an 'Integral' value from a 'YamlLight'.
yamlInt :: Integral b => YamlLight -> Maybe b
yamlInt (YStr s) = I.readSigned I.readDecimal s >>= noRemainder
yamlInt _ = Nothing

-- | Try to parse a 'Fractional' value from a 'YamlLight'.
yamlReal :: Fractional b => YamlLight -> Maybe b
yamlReal (YStr s) = F.readSigned F.readDecimal s >>= noRemainder
yamlReal _ = Nothing

-- | Lens into a sequence.
--
-- >>> YSeq [YStr "a", YStr "b", YStr "c"] ^? nth 1
-- Just (YStr "b")
--
-- >>> YSeq [YStr "a", YStr "b", YStr "c"] & nth 1 .~ YStr "B"
-- YSeq [YStr "a",YStr "B",YStr "c"]
--
-- >>> YSeq [YStr "a", YStr "b", YStr "c"] ^? nth 2 . _Yaml :: Maybe String
-- Just "c"
nth :: Int -> Traversal' YamlLight YamlLight
nth = ix . ArrIx

-- | Lens into a mapping. 'ByteString's are used as keys directly. If
-- you wish to use a complex mapping key, see 'key''.
--
-- >>> let m = YMap $ Map.fromList [(YStr "name", YStr "Tony Stark"), (YStr "sequels", YStr "2")]
-- >>> m & key "sequels" . _Yaml +~ 1
-- YMap (fromList [(YStr "name",YStr "Tony Stark"),(YStr "sequels",YStr "3")])
key :: ByteString -> Traversal' YamlLight YamlLight
key = key' . YStr

-- | Lens into a mapping using a complex key.
key' :: YamlLight -> Traversal' YamlLight YamlLight
key' = ix . ObjIx

-- | Convert between YAML values and common types of Haskell values.
class AsYaml a where
  fromYaml :: YamlLight -> Maybe a
  toYaml   :: a -> YamlLight

instance AsYaml (Map YamlLight YamlLight) where
  fromYaml (YMap m) = Just m
  fromYaml _        = Nothing
  toYaml = YMap

instance AsYaml [YamlLight] where
  fromYaml (YSeq a) = Just a
  fromYaml _        = Nothing
  toYaml = YSeq

instance AsYaml ByteString where
  fromYaml (YStr s) = Just s
  fromYaml _        = Nothing
  toYaml = YStr

instance AsYaml String where
  fromYaml (YStr s) = Just $ BC.unpack s
  fromYaml _        = Nothing
  toYaml = YStr . BC.pack

instance AsYaml Int where
  fromYaml x@(YStr _) = yamlInt x
  fromYaml _ = Nothing
  toYaml x = YStr $ if x < 0 then BC.cons '-' bs else bs
    where Just bs = I.packDecimal $ abs x
  -- toYaml = YStr . BC.pack . show

instance AsYaml Integer where
  fromYaml x@(YStr _) = yamlInt x
  fromYaml _ = Nothing
  toYaml x = YStr $ if x < 0 then BC.cons '-' bs else bs
    where Just bs = I.packDecimal $ abs x
  -- toYaml = YStr . BC.pack . show

instance AsYaml Double where
  fromYaml x@(YStr _) = yamlReal x
  fromYaml _ = Nothing
  toYaml = YStr . BC.pack . show

instance AsYaml Bool where
  fromYaml (YStr s) = case () of
                        _ | s == BC.pack "true"  -> Just True
                          | s == BC.pack "false" -> Just False
                          | otherwise            -> Nothing
  fromYaml _ = Nothing
  toYaml True  = YStr $ BC.pack "true"
  toYaml False = YStr $ BC.pack "false"

-- | Convert between YAML values and corresponding common Haskell
-- values.
--
-- >>> YStr "-2.3" ^? _Yaml :: Maybe Double
-- Just (-2.3)
--
-- >>> YStr "7b.3" ^? _Yaml :: Maybe Double
-- Nothing
--
-- >>> YStr "-23" ^? _Yaml :: Maybe Int
-- Just (-23)
--
-- >>> YStr "Help, I'm trapped in a haddock factory!" ^? _Yaml :: Maybe String
-- Just "Help, I'm trapped in a haddock factory!"
--
-- >>> YStr "An integer" ^? _Yaml :: Maybe Integer
-- Nothing
--
-- If we just want to pull out those values that were successfully
-- parsed,
-- 
-- >>> let nums = YSeq [YStr "3", YStr "2a", YStr "1"]
-- >>> nums ^.. each._Yaml :: [Int]
-- [3,1]
--
-- Alternately, we may want to fail the entire parse if any element
-- fails to parse.
-- 
-- >>> sequenceA $ map (preview _Yaml) (nums ^.. each) :: Maybe [Int]
-- Nothing
-- >>> let nums' = YSeq [YStr "3", YStr "2", YStr "1"]
-- >>> sequenceA $ map (preview _Yaml) (nums' ^.. each) :: Maybe [Int]
-- Just [3,2,1]
_Yaml :: AsYaml a => Prism' YamlLight a
_Yaml = prism' toYaml fromYaml

