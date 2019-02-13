{-# LANGUAGE TupleSections #-}

module LocationMap
  ( LocationMap
  , add
  , containsId
  , containsName
  , empty
  , isEmpty
  , findById
  , findByName
  ) where

import Api (Location (..))
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Data.Map.Strict as Map

data LocationMap = LocationMap
  { locationIdToNameMap :: Map Integer Text
  , locationNameToIdMap :: Map Text Integer
  }

add :: LocationMap -> Text -> (LocationMap, Location)
add map name' =
    maybe (map', location') (map, ) $ findByName map name'
  where
    id' = nextId map
    location' = Location id' name'
    locationIdToNameMap' = Map.insert id' name' $ locationIdToNameMap map
    locationNameToIdMap' = Map.insert name' id' $ locationNameToIdMap map
    map' = LocationMap locationIdToNameMap' locationNameToIdMap'

containsId :: LocationMap -> Integer -> Bool
containsId map key = Map.member key (locationIdToNameMap map)

containsName :: LocationMap -> Text -> Bool
containsName map key = Map.member key (locationNameToIdMap map)

empty :: LocationMap
empty = LocationMap mempty mempty

isEmpty :: LocationMap -> Bool
isEmpty map = Map.null (locationIdToNameMap map)

findById :: LocationMap -> Integer -> Maybe Location
findById map key = Location key <$>
  Map.lookup key (locationIdToNameMap map)

findByName :: LocationMap -> Text -> Maybe Location
findByName map key = flip Location key <$>
  Map.lookup key (locationNameToIdMap map)

nextId :: LocationMap -> Integer
nextId map = maybe 0 (succ . fst . fst) $
  Map.maxViewWithKey (locationIdToNameMap map)

