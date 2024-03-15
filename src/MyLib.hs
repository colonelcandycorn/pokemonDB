{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards   #-}

module MyLib where

import Prelude hiding (sum, id)
import           Opaleye (Field, FieldNullable, matchNullable, isNull,
                         Table, table, tableField, selectTable,
                         Select, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, sqlString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runSelect,
                         showSql, where_, Unpackspec,
                         SqlInt4, SqlInt8, SqlText, SqlDate, SqlFloat8, SqlBool,
                         sqlInt4, sqlInt8, sqlBool, toFields, Insert(..), rCount)
import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)
import           GenericData
import           ApiProcessing

import qualified Database.PostgreSQL.Simple as PGS
import Data.Profunctor.Product.Internal.TH (xTuple)
import Data.Int (Int64)
import Data.Aeson.KeyMap (insert)

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

-- data Test' a b = Test { testNumbers :: a, testWords :: b } deriving (Show)
-- type Test = Test' Int String
-- type TestField = Test' (Field SqlInt4) (Field SqlText)

-- $(makeAdaptorAndInstance "pTest" ''Test')

-- testTable :: Table TestField TestField
-- testTable = table "test"
--                   (pTest Test 
--                   { testNumbers = tableField "numbers"
--                   , testWords   = tableField "words"})

-- testSelect :: Select TestField
-- testSelect = selectTable testTable

-- runTestSelect :: PGS.Connection
--               -> Select TestField
--               -> IO [Test]
-- runTestSelect = runSelect

type PokemonSqlField = PokemonSql' (Field SqlInt4) (Field SqlText) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (Field SqlInt4) (FieldNullable SqlInt4)

type EggGroupSqlField = EggGroupSql' (Field SqlInt4) (Field SqlText)

type PokemonSpeciesSqlField = PokemonSpeciesSql' (Field SqlInt4) (Field SqlBool) (Field SqlBool) (FieldNullable SqlInt4) (Field SqlInt4) (Field SqlText)

type ActualMoveSqlField = ActualMoveSql' (Field SqlInt4) (Field SqlText) (Field SqlInt4) (FieldNullable SqlInt4) (FieldNullable SqlInt4) (FieldNullable SqlInt4)

type MovePokemonRelationSqlField = MovePokemonRelationSql' (Field SqlInt4) (Field SqlInt4)

type ActualAbilitySqlField = ActualAbilitySql' (Field SqlInt4) (Field SqlText)

type AbilityPokemonRelationSqlField = AbilityPokemonRelationSql' (Field SqlInt4) (Field SqlInt4)

type TypeSqlField = TypeSql' (Field SqlInt4) (Field SqlText)

type TypeDoubleDamageSqlField = TypeDoubleDamageSql' (Field SqlInt4) (Field SqlInt4)

type TypeHalfDamageSqlField = TypeHalfDamageSql' (Field SqlInt4) (Field SqlInt4)

type TypeNoDamageSqlField = TypeNoDamageSql' (Field SqlInt4) (Field SqlInt4)


$(makeAdaptorAndInstance "pPokemonSql" ''PokemonSql')

$(makeAdaptorAndInstance "pEggGroupSql" ''EggGroupSql')

$(makeAdaptorAndInstance "pPokemonSpeciesSql" ''PokemonSpeciesSql')

$(makeAdaptorAndInstance "pActualMoveSql" ''ActualMoveSql')

$(makeAdaptorAndInstance "pMovePokemonRelationSql" ''MovePokemonRelationSql')

$(makeAdaptorAndInstance "pActualAbilitySql" ''ActualAbilitySql')

$(makeAdaptorAndInstance "pAbilityPokemonRelationSql" ''AbilityPokemonRelationSql')

$(makeAdaptorAndInstance "pTypeSql" ''TypeSql')

$(makeAdaptorAndInstance "pTypeDoubleSql" ''TypeDoubleDamageSql')

$(makeAdaptorAndInstance "pTypeHalfDamageSql" ''TypeHalfDamageSql')

$(makeAdaptorAndInstance "pTypeNoDamageSql" ''TypeNoDamageSql')



pokemonTable :: Table PokemonSqlField PokemonSqlField
pokemonTable = table "pokemon"
                  (pPokemonSql PokemonSql'
                  { sqlId = tableField "id"
                  , sqlPokemonName = tableField "pokemon_name"
                  , sqlSpecies = tableField "species"
                  , sqlHp = tableField "hp"
                  , sqlAttack = tableField "attack"
                  , sqlDefense = tableField "defense"
                  , sqlSpecialAttack = tableField "special_attack"
                  , sqlSpecialDefense = tableField "special_defense"
                  , sqlSpeed = tableField "speed"
                  , sqlFirstType = tableField "first_type"
                  , sqlSecondType = tableField "second_type"})


eggGroupTable :: Table EggGroupSqlField EggGroupSqlField
eggGroupTable = table "egggrouprel"
                  (pEggGroupSql EggGroupSql'
                  { pokemonId = tableField "pokemon_id"
                  , eggGroupName = tableField "egggroup_name"})


pokemonSpeciesTable :: Table PokemonSpeciesSqlField PokemonSpeciesSqlField
pokemonSpeciesTable = table "pokemon_species"
                  (pPokemonSpeciesSql PokemonSpeciesSql'
                  { sqlSpeciesId = tableField "species_id"
                  , sqlIsLegendary = tableField "is_legendary"
                  , sqlIsMythical = tableField "is_mythical"
                  , sqlEvolutionFrom = tableField "evolves_from"
                  , sqlCaptureRate = tableField "capture_rate"
                  , sqlSpeciesName = tableField "species_name"})


actualMoveTable :: Table ActualMoveSqlField ActualMoveSqlField
actualMoveTable = table "move"
                  (pActualMoveSql ActualMoveSql'
                  { sqlMoveId = tableField "move_id"
                  , sqlMoveName = tableField "move_name"
                  , sqlMoveType = tableField "move_type"
                  , sqlMovePower = tableField "move_power"
                  , sqlMovePP = tableField "move_pp"
                  , sqlMoveAccuracy = tableField "move_accuracy"})


movePokemonRelationTable :: Table MovePokemonRelationSqlField MovePokemonRelationSqlField
movePokemonRelationTable = table "move_pokemon_relation"
                  (pMovePokemonRelationSql MovePokemonRelationSql'
                  { moveId = tableField "move_id"
                  , pokemonWhoLearns = tableField "pokemon_who_learns"})


abilityPokemonRelationTable :: Table AbilityPokemonRelationSqlField AbilityPokemonRelationSqlField
abilityPokemonRelationTable = table "ability_pokemon_relation"
                  (pAbilityPokemonRelationSql AbilityPokemonRelationSql'
                  { abilityId = tableField "ability_id"
                  , sqlAbilityPokemonId = tableField "pokemon_id"})

actualAbilityTable :: Table ActualAbilitySqlField ActualAbilitySqlField
actualAbilityTable = table "ability"
                  (pActualAbilitySql ActualAbilitySql'
                  { sqlAbilityId = tableField "ability_id"
                  , sqlAbilityName = tableField "ability_name"})

typeTable :: Table TypeSqlField TypeSqlField
typeTable = table "type"
                  (pTypeSql TypeSql'
                  { typeId = tableField "type_id"
                  , typeName = tableField "type_name"})

typeDoubleDamageTable :: Table TypeDoubleDamageSqlField TypeDoubleDamageSqlField
typeDoubleDamageTable = table "type_double_damage"
                  (pTypeDoubleSql TypeDoubleDamageSql'
                  { doubleDamageTypeId = tableField "type_id"
                  , doubleDamageToTypeId = tableField "damage_type_id"})

typeHalfDamageTable :: Table TypeHalfDamageSqlField TypeHalfDamageSqlField
typeHalfDamageTable = table "type_half_damage"
                  (pTypeHalfDamageSql TypeHalfDamageSql'
                  { halfDamageTypeId = tableField "type_id"
                  , halfDamageToTypeId = tableField "damage_type_id"})

typeNoDamageTable :: Table TypeNoDamageSqlField TypeNoDamageSqlField
typeNoDamageTable = table "type_no_damage"
                  (pTypeNoDamageSql TypeNoDamageSql'
                  { noDamageTypeId = tableField "type_id"
                  , noDamageToTypeId = tableField "damage_type_id"})


insertAbilityPokemonRelation :: AbilityPokemonRelationSql -> Insert Int64
insertAbilityPokemonRelation x = Insert
    { iTable = abilityPokemonRelationTable
    , iRows = [abilityPokemonRelationSqlToAbilityPokemonRelationSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

abilityPokemonRelationSqlToAbilityPokemonRelationSqlField :: AbilityPokemonRelationSql -> AbilityPokemonRelationSqlField
abilityPokemonRelationSqlToAbilityPokemonRelationSqlField AbilityPokemonRelationSql'{..} = AbilityPokemonRelationSql'
  { abilityId = sqlInt4 abilityId
  , sqlAbilityPokemonId = sqlInt4 sqlAbilityPokemonId
  }

insertTypeNoDamage :: TypeNoDamageSql -> Insert Int64
insertTypeNoDamage x = Insert
    { iTable = typeNoDamageTable
    , iRows = [typeNoDamageSqlToSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

typeNoDamageSqlToSqlField :: TypeNoDamageSql -> TypeNoDamageSqlField
typeNoDamageSqlToSqlField TypeNoDamageSql'{..} = TypeNoDamageSql'
  { noDamageTypeId = sqlInt4 noDamageTypeId
  , noDamageToTypeId= sqlInt4 noDamageToTypeId
  }

insertTypeHalfDamage :: TypeHalfDamageSql -> Insert Int64
insertTypeHalfDamage x = Insert
    { iTable = typeHalfDamageTable
    , iRows = [typeHalfDamageSqlToSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

typeHalfDamageSqlToSqlField :: TypeHalfDamageSql -> TypeHalfDamageSqlField
typeHalfDamageSqlToSqlField TypeHalfDamageSql'{..} = TypeHalfDamageSql'
  { halfDamageTypeId = sqlInt4 halfDamageTypeId
  , halfDamageToTypeId = sqlInt4 halfDamageToTypeId
  }

insertTypeDoubleDamage :: TypeDoubleDamageSql -> Insert Int64
insertTypeDoubleDamage x = Insert
    { iTable = typeDoubleDamageTable
    , iRows = [typeDoubleDamageSqlToSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

typeDoubleDamageSqlToSqlField :: TypeDoubleDamageSql -> TypeDoubleDamageSqlField
typeDoubleDamageSqlToSqlField TypeDoubleDamageSql'{..} = TypeDoubleDamageSql'
    { doubleDamageTypeId = sqlInt4 doubleDamageTypeId
    , doubleDamageToTypeId = sqlInt4 doubleDamageToTypeId
    }

insertType :: Type -> Insert Int64
insertType x = Insert
    { iTable = typeTable
    , iRows = [typeSqlToTypeSqlField . typeToTypeSql $ x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

typeSqlToTypeSqlField :: TypeSql -> TypeSqlField
typeSqlToTypeSqlField TypeSql'{..} = TypeSql'
  { typeId = sqlInt4 typeId
  , typeName = sqlString typeName
  }

insertActualAbility :: ActualAbility -> Insert Int64
insertActualAbility x = Insert
    { iTable = actualAbilityTable
    , iRows = [actualAbilitySqlToActualAbilitySqlField . actualAbilityToActualAbilitySql $ x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }

actualAbilitySqlToActualAbilitySqlField :: ActualAbilitySql -> ActualAbilitySqlField
actualAbilitySqlToActualAbilitySqlField ActualAbilitySql'{..} = ActualAbilitySql'
  { sqlAbilityId = sqlInt4 sqlAbilityId
  , sqlAbilityName = sqlString sqlAbilityName
  }

pokemonSelect :: Select PokemonSqlField
pokemonSelect = selectTable pokemonTable

insertPokemon :: Pokemon -> Insert Int64
insertPokemon x = Insert
    { iTable = pokemonTable
    , iRows = [pokemonSqlToPokemonSqlField . pokemonToPokemonSql $ x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }


insertSpecies :: Species -> Insert Int64
insertSpecies x = Insert
    { iTable = pokemonSpeciesTable
    , iRows = [pokemonSpeciesSqlToPokemonSpeciesSqlField . speciesToPokemonSpeciesSql $ x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }




actualMoveToMoveRelationSql :: ActualMove -> [MovePokemonRelationSql]
actualMoveToMoveRelationSql x = 
    let moveId' = moveID x
        pokemonWhoLearn' = map (getIDFromURL . url ) $  learnedBy x
    in map (MovePokemonRelationSql' moveId') pokemonWhoLearn'

moveRelationSqlToMoveRelationSqlField :: MovePokemonRelationSql -> MovePokemonRelationSqlField
moveRelationSqlToMoveRelationSqlField MovePokemonRelationSql'{..} = MovePokemonRelationSql'
  { moveId = sqlInt4 moveId
  , pokemonWhoLearns = sqlInt4 pokemonWhoLearns
  }

insertMoveRelation :: MovePokemonRelationSql -> Insert Int64
insertMoveRelation x = Insert
    { iTable = movePokemonRelationTable
    , iRows = [moveRelationSqlToMoveRelationSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }


actualMoveToActualMoveSql :: ActualMove -> ActualMoveSql
actualMoveToActualMoveSql x = 
    let moveId' = moveID x
        moveName' = moveName x
        moveType' = getIDFromURL . url . typeAPI $ x
        movePower' = power x
        movePP' = pp x
        moveAccuracy' = accuracy x
    in ActualMoveSql' moveId' moveName' moveType' movePower' movePP' moveAccuracy'

actualMoveSqlToActualMoveSqlField :: ActualMoveSql -> ActualMoveSqlField
actualMoveSqlToActualMoveSqlField ActualMoveSql'{..} = 
    ActualMoveSql'
    { sqlMoveId = sqlInt4 sqlMoveId
    , sqlMoveName = sqlString sqlMoveName
    , sqlMoveType = sqlInt4 sqlMoveType
    , sqlMovePower = toFields sqlMovePower
    , sqlMovePP = toFields sqlMovePP
    , sqlMoveAccuracy = toFields sqlMoveAccuracy
    }

insertActualMove :: ActualMove -> Insert Int64
insertActualMove x = Insert
    { iTable = actualMoveTable
    , iRows = [actualMoveSqlToActualMoveSqlField . actualMoveToActualMoveSql $ x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }


speciesToPokemonSpeciesSql :: Species -> PokemonSpeciesSql
speciesToPokemonSpeciesSql x =
    let evolution= case evolutionFrom x of
            Just y -> Just . getIDFromURL . url $ y
            _ -> Nothing
    in PokemonSpeciesSql'
    { sqlSpeciesId = pokemonSpeciesId x
    , sqlIsLegendary = isLegendary x
    , sqlIsMythical = isMythical x
    , sqlEvolutionFrom = evolution
    , sqlCaptureRate = captureRate x
    , sqlSpeciesName = speciesName x
    }

pokemonSpeciesSqlToPokemonSpeciesSqlField :: PokemonSpeciesSql -> PokemonSpeciesSqlField
pokemonSpeciesSqlToPokemonSpeciesSqlField PokemonSpeciesSql'{..} = PokemonSpeciesSql'
  { sqlSpeciesId = sqlInt4 sqlSpeciesId
  , sqlIsLegendary = sqlBool sqlIsLegendary
  , sqlIsMythical = sqlBool sqlIsMythical
  , sqlEvolutionFrom = toFields sqlEvolutionFrom
  , sqlCaptureRate = sqlInt4 sqlCaptureRate
  , sqlSpeciesName = sqlString sqlSpeciesName
  }

speciesToEggGroupSql :: Species -> [EggGroupSql]
speciesToEggGroupSql x =
    let pokemonId = pokemonSpeciesId x
        eggNamedAPI = eggGroups x
    in map (EggGroupSql' pokemonId . resourceName) eggNamedAPI
-- pokemonToStat :: Pokemon -> Stat
-- pokemonToStat Pokemon{..} = Stat


eggGroupSqlToEggGroupSqlField :: EggGroupSql -> EggGroupSqlField
eggGroupSqlToEggGroupSqlField EggGroupSql'{..} = EggGroupSql'
    { pokemonId = sqlInt4 pokemonId
    , eggGroupName = sqlString eggGroupName
    }

insertEggGroup :: EggGroupSql -> Insert Int64
insertEggGroup x = Insert
    { iTable = eggGroupTable
    , iRows = [eggGroupSqlToEggGroupSqlField x]
    , iReturning = rCount
    , iOnConflict = Nothing
    }


-- data Pokemon = Pokemon 
--   { id     :: Int
--   , pokemonName   :: String
--   , species :: NamedAPIResource
--   , stats :: [Stat]
--   } deriving (Show)

--   data PokemonSql' a b c d e f g h i = PokemonSql' 
--   { id :: a
--   , pokemonName   :: b
--   , species :: c
--   , hp :: d
--   , attack :: e
--   , defense :: f
--   , specialAttack :: g
--   , specialDefense :: h
--   , speed :: i
--   } deriving (Show)

-- type PokemonSql = PokemonSql' Int String String Int Int Int Int Int Int

pokemonToPokemonSql :: Pokemon -> PokemonSql 
pokemonToPokemonSql x =
    let statBreakdown = statsToStatBreakdown . stats $ x 
        types = pokemonTypeApiToTypeSql . typeList $ x
    in PokemonSql'
    { sqlId = id x
    , sqlPokemonName = pokemonName x 
    , sqlSpecies = getIDFromURL . url . species $ x
    , sqlHp = hp statBreakdown 
    , sqlAttack = attack statBreakdown 
    , sqlDefense = defense statBreakdown
    , sqlSpecialAttack = specialAttack statBreakdown 
    , sqlSpecialDefense = specialDefense statBreakdown 
    , sqlSpeed = speed statBreakdown
    , sqlFirstType = fst types
    , sqlSecondType = snd types
    }

pokemonSqlToPokemonSqlField :: PokemonSql -> PokemonSqlField
pokemonSqlToPokemonSqlField PokemonSql'{..} = PokemonSql'
  { sqlId = sqlInt4 sqlId
  , sqlPokemonName = sqlString sqlPokemonName
  , sqlSpecies = sqlInt4 sqlSpecies
  , sqlHp = sqlInt4 sqlHp
  , sqlAttack = sqlInt4 sqlAttack
  , sqlDefense = sqlInt4 sqlDefense
  , sqlSpecialAttack = sqlInt4 sqlSpecialAttack
  , sqlSpecialDefense = sqlInt4 sqlSpecialDefense
  , sqlSpeed = sqlInt4 sqlSpeed
  , sqlFirstType = sqlInt4 sqlFirstType
  , sqlSecondType = toFields sqlSecondType
  }

pokemonTypeApiToTypeSql :: [TypeApi] -> (Int, Maybe Int)
pokemonTypeApiToTypeSql [] = (0, Nothing)
pokemonTypeApiToTypeSql [x] = (getIDFromURL . url . typeApiResource $ x, Nothing)
pokemonTypeApiToTypeSql (x:y:_) = (getIDFromURL . url . typeApiResource $ x, Just . getIDFromURL . url . typeApiResource $ y)

typeToTypeSql :: Type -> TypeSql
typeToTypeSql x = TypeSql'
    { typeId = ApiProcessing.typeId x
    , typeName = ApiProcessing.typeName x
    }

typeToTypeDoubleDamageSql :: Type -> [TypeDoubleDamageSql]
typeToTypeDoubleDamageSql x = 
    let typeId' = ApiProcessing.typeId x
        doubleDamageTo' = map (TypeDoubleDamageSql' typeId' . getIDFromURL . url) $ (doubleDamageTo . damageRelations) x
    in doubleDamageTo'

typeToHalfDamageSql :: Type -> [TypeHalfDamageSql]
typeToHalfDamageSql x = 
    let typeId' = ApiProcessing.typeId x
        halfDamageTo' = map (TypeHalfDamageSql' typeId' . getIDFromURL . url) $ (halfDamageTo . damageRelations) x
    in halfDamageTo'

typeToNoDamageSql :: Type -> [TypeNoDamageSql]
typeToNoDamageSql x = 
    let typeId' = ApiProcessing.typeId x
        noDamageTo' = map (TypeNoDamageSql' typeId' . getIDFromURL . url) $ (noDamageTo . damageRelations) x
    in noDamageTo'

actualAbilityToActualAbilitySql :: ActualAbility -> ActualAbilitySql
actualAbilityToActualAbilitySql x = ActualAbilitySql'
    { sqlAbilityId = ApiProcessing.abilityId x 
    , sqlAbilityName = abilityName x
    }

actualAbilityToAbilityPokemonRelationSql :: ActualAbility -> [AbilityPokemonRelationSql]
actualAbilityToAbilityPokemonRelationSql x = 
    let abilityId' = ApiProcessing.abilityId x
        pokemonId' = map (AbilityPokemonRelationSql' abilityId' . getIDFromURL . url . pokemonAPI ) $ pokemonLearnedBy x
    in pokemonId'