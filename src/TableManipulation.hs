

module TableManipulation where

import MyLib
import ApiProcessing
import Network.HTTP.Simple
import GenericData
import Database.PostgreSQL.Simple as PGS
import qualified Opaleye
import Opaleye (sqlString)
import Data.Int (Int64)
import Data.Maybe (fromJust)




insertPokemonList :: [Pokemon] -> PGS.Connection -> IO Int64
insertPokemonList xs conn =
    insertPokemonList' xs conn 0
    where
      insertPokemonList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertPokemon x)
        insertPokemonList' xs conn (n + res)
      insertPokemonList' [] _ n = return n


insertSpeciesAndEggGroup :: [Species] -> PGS.Connection -> IO Int64
insertSpeciesAndEggGroup xs conn = do
  let eggGroups = concatMap speciesToEggGroupSql xs
  speciesInserted <- insertSpeciesList xs conn
  eggGroupsInserted <- insertEggGroupList eggGroups conn
  return (speciesInserted + eggGroupsInserted)


insertTypeAndVariousTypeRelations :: [Type] -> PGS.Connection -> IO Int64
insertTypeAndVariousTypeRelations xs conn = do
  let doubleDamageTo = concatMap typeToTypeDoubleDamageSql xs
  let halfDamageTo = concatMap typeToHalfDamageSql xs
  let noDamageTo = concatMap typeToNoDamageSql xs
  typeInserted <- insertTypeList xs conn
  doubleDamageToInserted <- insertDoubleDamageRelationList doubleDamageTo conn
  halfDamageToInserted <- insertHalfDamageRelationList halfDamageTo conn
  noDamageToInserted <- insertNoDamageRelationList noDamageTo conn
  return (typeInserted + doubleDamageToInserted + halfDamageToInserted + noDamageToInserted)


insertTypeList :: [Type] -> PGS.Connection -> IO Int64
insertTypeList xs conn =
    insertTypeList' xs conn 0
    where
      insertTypeList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertType x)
        insertTypeList' xs conn (n + res)
      insertTypeList' [] _ n = return n

insertDoubleDamageRelationList :: [TypeDoubleDamageSql] -> PGS.Connection -> IO Int64
insertDoubleDamageRelationList xs conn =
    insertDoubleDamageRelationList' xs conn 0
    where
      insertDoubleDamageRelationList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertTypeDoubleDamage x)
        insertDoubleDamageRelationList' xs conn (n + res)
      insertDoubleDamageRelationList' [] _ n = return n

insertHalfDamageRelationList :: [TypeHalfDamageSql] -> PGS.Connection -> IO Int64
insertHalfDamageRelationList xs conn =
    insertHalfDamageRelationList' xs conn 0
    where
      insertHalfDamageRelationList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertTypeHalfDamage x)
        insertHalfDamageRelationList' xs conn (n + res)
      insertHalfDamageRelationList' [] _ n = return n

insertNoDamageRelationList :: [TypeNoDamageSql] -> PGS.Connection -> IO Int64
insertNoDamageRelationList xs conn =
    insertNoDamageRelationList' xs conn 0
    where
      insertNoDamageRelationList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertTypeNoDamage x)
        insertNoDamageRelationList' xs conn (n + res)
      insertNoDamageRelationList' [] _ n = return n





insertAbilityAndAbilityPokemonRelation :: [ActualAbility] -> PGS.Connection -> IO Int64
insertAbilityAndAbilityPokemonRelation xs conn = do
  let abilityPokemonRelations = concatMap actualAbilityToAbilityPokemonRelationSql xs
  actualAbilitiesInserted <- insertActualAbilityList xs conn
  abilityPokemonRelationsInserted <- insertAbilityPokemonRelationList abilityPokemonRelations conn
  return (actualAbilitiesInserted + abilityPokemonRelationsInserted)

insertActualAbilityList :: [ActualAbility] -> PGS.Connection -> IO Int64
insertActualAbilityList xs conn =
    insertActualAbilityList' xs conn 0
    where
      insertActualAbilityList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertActualAbility x)
        insertActualAbilityList' xs conn (n + res)
      insertActualAbilityList' [] _ n = return n

insertAbilityPokemonRelationList :: [AbilityPokemonRelationSql] -> PGS.Connection -> IO Int64
insertAbilityPokemonRelationList xs conn =
    insertAbilityPokemonRelationList' xs conn 0
    where
      insertAbilityPokemonRelationList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertAbilityPokemonRelation x)
        insertAbilityPokemonRelationList' xs conn (n + res)
      insertAbilityPokemonRelationList' [] _ n = return n

insertActualMoveAndMovePokemonRelation :: [ActualMove] -> PGS.Connection -> IO Int64
insertActualMoveAndMovePokemonRelation xs conn = do
  let movePokemonRelations = concatMap actualMoveToMoveRelationSql xs
  actualMovesInserted <- insertActualMoveList xs conn
  movePokemonRelationsInserted <- insertMovePokemonRelationList movePokemonRelations conn
  return (actualMovesInserted + movePokemonRelationsInserted)

insertActualMoveList :: [ActualMove] -> PGS.Connection -> IO Int64
insertActualMoveList xs conn =
    insertActualMoveList' xs conn 0
    where
      insertActualMoveList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertActualMove x)
        insertActualMoveList' xs conn (n + res)
      insertActualMoveList' [] _ n = return n

insertMovePokemonRelationList :: [MovePokemonRelationSql] -> PGS.Connection -> IO Int64
insertMovePokemonRelationList xs conn =
    insertMovePokemonRelationList' xs conn 0
    where
      insertMovePokemonRelationList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertMoveRelation x)
        insertMovePokemonRelationList' xs conn (n + res)
      insertMovePokemonRelationList' [] _ n = return n

insertEggGroupList :: [EggGroupSql] -> PGS.Connection -> IO Int64
insertEggGroupList xs conn =
    insertEggGroupList' xs conn 0
    where
      insertEggGroupList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertEggGroup x)
        insertEggGroupList' xs conn (n + res)
      insertEggGroupList' [] _ n = return n

insertSpeciesList :: [Species] -> PGS.Connection -> IO Int64
insertSpeciesList xs conn =
    insertSpeciesList' xs conn 0
    where
      insertSpeciesList' (x:xs) conn n = do
        res <- Opaleye.runInsert conn (insertSpecies x)
        insertSpeciesList' xs conn (n + res)
      insertSpeciesList' [] _ n = return n


stringToRequests :: [String] -> Maybe [Request]
stringToRequests = mapM parseRequest 

getNPokemon :: Int -> [Request]
getNPokemon n = case stringToRequests ["https://pokeapi.co/api/v2/pokemon/" <> show val | val <- [1..n]] of
  Just lst -> lst
  _ -> []

getNSpecies :: Int -> Int -> [Request]
getNSpecies x y = case stringToRequests ["https://pokeapi.co/api/v2/pokemon-species/" <> show val | val <- [x..y]] of
  Just lst -> lst
  _ -> []

getNMoves :: Int -> Int -> [Request]
getNMoves x y = case stringToRequests ["https://pokeapi.co/api/v2/move/" <> show val | val <- [x..y]] of
  Just lst -> lst
  _ -> []


convertStringToSqlText :: String -> Opaleye.Field Opaleye.SqlText
convertStringToSqlText = sqlString


getNAbilities :: Int -> Int -> [Request]
getNAbilities x y = case stringToRequests ["https://pokeapi.co/api/v2/ability/" <> show val | val <- [x..y]] of
  Just lst -> lst
  _ -> []

getNTypes :: Int -> Int -> [Request]
getNTypes x y = case stringToRequests ["https://pokeapi.co/api/v2/type/" <> show val | val <- [x..y]] of
  Just lst -> lst
  _ -> []